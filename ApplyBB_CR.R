rm(list=ls())
direct<-"C:\\NFT\\AGEPROV40\\Haddock"
setwd(direct)
proj.fname<-"GBH_2017UPDATE_4YR" #AgePro name to work from

#input control rule parameters and MSY reference points
FracBmsyThreshHi=c(0.0) # For control rule; Threshold as fraction of Bmsy to switch from Fmsy*FracFtarg as target F to linear decline to zero
FracBmsyThreshLo=c(0.0) #For control rule; Level of SSB as fraction of Bmsy where target F set to 0
FracFtarg<-c(1) #fraction of Fmsy that serves as max target F in control rule
Bmsy<-104300
Fmsy<-0.57

################################################################
#Read in an agepro input for manipulation later
input <- readLines(con=(paste(direct,paste(proj.fname,'INP',sep='.'),sep="\\"))) #read starting agepro file
fyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],1,4)) #ID first year from Input file
source(paste(direct,"Analyze_AgePro.R",sep="\\")) #function that will analyze age pro results (e.g., calc median SSB, etc.)

#Run agepro using intial input file; just a starting point.
agepro.run<- shell(paste("  agepro40  ", paste0(proj.fname,".INP"), sep=""), mustWork=F, intern=T )

#Need status in year one of projection to apply cr in loop below
ssb <- read.table(paste(direct,paste(proj.fname,'xx3',sep='.'),sep="\\"))
colnames(ssb) <- seq(fyr,fyr+ncol(ssb)-1)
ssb.median <- apply(ssb,2,median)
SSBlevels<-ssb.median/Bmsy #relative to Bmsy

#extract the element of input file where F or catch values specified
harvscen<-input[which(input == "[HARVEST]")+2]
harvscen<-unlist(strsplit(harvscen,split=" ")) #data manip so I can change harvest scenario
harvscen<-harvscen[harvscen != ""]
#########################################
for(s in 1:(length(SSBlevels)-1)){
  if(s>1) {
  #Get status from previous age pro run and then reapply CR and run agepro again
  harvscen<-unlist(strsplit(harvscen,split=" ")) #data manip so we can change one element of harvscen
  ssb<-read.table(paste(direct,paste(paste0(proj.fname,s-1),'xx3',sep='.'),sep="\\"))
  colnames(ssb) <- seq(fyr,fyr+ncol(ssb)-1)
  ssb.median <- apply(ssb,2,median)
  SSBlevels<-ssb.median/Bmsy
  }
  #apply CR
  Fmult<-BB_CR(SSBstatus=SSBlevels[s],SSBthresh=FracBmsyThreshHi,SSBthreshlo = FracBmsyThreshLo,FracFtarg=FracFtarg,Fmsy=Fmsy,CtrlRule=1) #call to HCR
  ifile <- paste(proj.fname,s,".INP", sep="") #name of new agepro
  write(input[1:(which(input == "[HARVEST]")+1)], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
  harvscen[s+1]<-round(Fmult,2)
  harvscen<-paste(harvscen,sep=" ",collapse=" ") #take multiple characters and make one string sep'd by a space; needed for agepro to read correctly
  write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
  write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
  ###Run AgePro with proj.fname_s.INP
  agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
}
annual<-AnaAgePro(proj.fname.b=paste0(proj.fname,s),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy)

##Run the final agepro at the catch levels associated with the F values to get P(overfishing)
catch <- read.table(paste(direct,paste(paste0(proj.fname,s),'xx6',sep='.'),sep="\\"))
colnames(catch) <- names(ssb)
catch.median <- apply(catch,2,median)
ifile <- paste(proj.fname,paste0(s,"b"),".INP", sep="") #name of new agepro
write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
harvscen.num<-rep(1,length(catch)) #change numbers here so agepro uses catch instead of fmult
write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
harvscen<-round(catch.median,0)
write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
annual.catch<-AnaAgePro(proj.fname.b=paste0(proj.fname,s,"b"),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy)
annual["P(overfishing)"]<-annual.catch["P(overfishing)"]
annual["SSB/SSBmsy"]<-round(annual$SSB/Bmsy,2)

rmarkdown::render(paste(direct,"ProjectionTables.Rmd",sep="\\"),output_file=paste0(proj.fname,s,"_annual.docx"),params=list( hcr=annual,title=paste(proj.fname,"annual",sep=" ")))

#Run each HCR with constant catch, i.e., catch in year one applies to all years
ifile <- paste(proj.fname,"4_concatch",".INP", sep="") #name of new agepro
write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
harvscen.num<-rep(1,length(catch))
write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
harvscen<-c(catch.median[1],rep(round(catch.median[2],0),(length(catch)-1)))
write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
threeblock<-AnaAgePro(proj.fname.b=paste0(proj.fname,"4_concatch"),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy)
threeblock["SSB/SSBmsy"]<-round(threeblock$SSB/Bmsy,2)

rmarkdown::render(paste(direct,"ProjectionTables.Rmd",sep="\\"),output_file=paste0(proj.fname,"4_concatch.docx"),params=list( hcr=threeblock,title=paste(proj.fname,"Constant Catch",sep=" ")))
