rm(list=ls())
direct<-"C:\\NFT\\AGEPROV40\\GBYT"
setwd(direct)
proj.fname<-"Fref025" #AgePro name to work from

#input control rule parameters and MSY reference points
FracBmsyThreshHi=c(0.0) # For control rule; Threshold as fraction of Bmsy to switch from Fmsy*FracFtarg as target F to linear decline to zero
FracBmsyThreshLo=c(0.0) #For control rule; Level of SSB as fraction of Bmsy where target F set to 0
FracFtarg<-c(1) #fraction of Fmsy that serves as max target F in control rule
Bmsy<-26800
Fmsy<-0.25
decimals<-3

###inputs for long-term MSY agepro run; if desired
domsy<-TRUE #Do the MSY, TRUE, or not, FALSE
msy.name<-"Fref025_long" #name of agepro
CIwant<-data.frame(low=0.05,hi=0.95) #What CIs do you want?
fmsyold<-999
Bmsyold<-999
msyold<-999
recrold<-999
nyr.avg<-10 #average how many of the last years of long-term proj for ref point?

################################################################
source(paste(direct,"Analyze_AgePro.R",sep="\\")) #function that will analyze age pro results (e.g., calc median SSB, etc.)

###Short-term projections and control rule application
#Read in an agepro input for manipulation later
input <- readLines(con=(paste(direct,paste(proj.fname,'INP',sep='.'),sep="\\"))) #read starting agepro file
fyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],1,4)) #ID first year from Input file

#Run agepro using intial input file; just a starting point.
agepro.run<- shell(paste("  agepro40  ", paste0(proj.fname,".INP"), sep=""), mustWork=F, intern=T )

#Need status in year one of projection to apply cr in loop below
ssb <- read.table(paste(direct,paste(proj.fname,'xx3',sep='.'),sep="\\"))
colnames(ssb) <- seq(fyr,fyr+ncol(ssb)-1)
ssb.median <- apply(ssb,2,median)
SSBlevels<-ssb.median/Bmsy #relative to Bmsy

#extract the element of input file where F or catch values specified
harvscen.num<-input[which(input == "[HARVEST]")+1]
harvscen.num<-unlist(strsplit(harvscen.num,split=" ")) #data manip so I can change harvest scenario
harvscen.num<-harvscen.num[harvscen.num != ""]
harvscen<-input[which(input == "[HARVEST]")+2]
harvscen<-unlist(strsplit(harvscen,split=" ")) #data manip so I can change harvest scenario
harvscen<-harvscen[harvscen != ""]
#Create OFL holder
OFL<-c()
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
  harvscen[s+1]<-round(Fmult,decimals)
  harvscen<-paste(harvscen,sep=" ",collapse=" ") #take multiple characters and make one string sep'd by a space; needed for agepro to read correctly
  write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
  write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
  ###Run AgePro with proj.fname_s.INP
  agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
  
  ##Do iterative OFL calculation
  ##get catches resulting from CR application
  if(s>1){
  catch.cr <- read.table(paste(direct,paste(paste0(proj.fname,s-1),'xx6',sep='.'),sep="\\"))
  colnames(catch.cr) <- seq(fyr,fyr+ncol(catch.cr)-1)
  catch.median.cr <- apply(catch.cr,2,median)
  }
  if(s==1){
    harvscen.ofl<-unlist(strsplit(harvscen,split=" "))
    harvscen.ofl[s+1]<-Fmsy
    OFL[s]<-"--"
  } else {
    harvscen.ofl[s]<-catch.median.cr[s]
    harvscen.ofl[s+1]<-Fmsy
  }
  OFL[s+1]<-OFLfxn(fyr=fyr,direct=direct,s=s,proj.fname=proj.fname,input=input,Fmsy=Fmsy,harvscen.num=harvscen.num,harvscen.ofl=harvscen.ofl)
  harvscen.num[s+1]<-1
}
annual<-AnaAgePro(proj.fname.b=paste0(proj.fname,s),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy,decimals=decimals)
#if F=Fmsy in all years>1 then catch should = OFL exactly, but won't due to differences
#in specifying F versus catch in agepro.  So if F=Fmsy in all years then OFL=catch; otherwise,
# use iterative solution from loops.
if(length(unique(annual$F[2:nrow(annual)]))==1 & annual$F[2]==Fmsy) {
  annual[2:nrow(annual),"OFL"]<-annual$Catch[2:nrow(annual)]
} else {
  annual["OFL"]<-OFL
}


##Run the final agepro at the catch levels associated with the F values to get P(overfishing)
catch <- read.table(paste(direct,paste(paste0(proj.fname,s),'xx6',sep='.'),sep="\\"))
colnames(catch) <- names(ssb)
catch.median <- apply(catch,2,median)
ifile <- paste(proj.fname,paste0(s,"b"),".INP", sep="") #name of new agepro
write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
harvscen.num<-rep(1,length(catch)) #change numbers here so agepro uses catch instead of fmult
write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
harvscen<-round(catch.median,4)
write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
annual.catch<-AnaAgePro(proj.fname.b=paste0(proj.fname,s,"b"),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy,decimals=decimals)
annual["P(overfishing)"]<-annual.catch["P(overfishing)"]
annual["SSB/SSBmsy"]<-round(annual$SSB/Bmsy,decimals)

rmarkdown::render(paste(direct,"ProjectionTables.Rmd",sep="\\"),output_file=paste0(proj.fname,"_annual.docx"),params=list( hcr=annual,title=paste(proj.fname,"annual",sep=" ")))

#Run each HCR with constant catch, i.e., catch in year one applies to all years
ifile <- paste(proj.fname,paste0(s,"_concatch"),".INP", sep="") #name of new agepro
write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
harvscen.num<-rep(1,length(catch))
write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
harvscen<-c(catch.median[1],rep(round(catch.median[2],0),(length(catch)-1)))
write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
threeblock<-AnaAgePro(proj.fname.b=paste0(proj.fname,s,"_concatch"),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy,decimals=decimals)

##Do iterative OFL calculation
#reset the harvscen and harvscen.num values (should be 1 and then 0's and replaced in loop below)
harvscen.num<-input[which(input == "[HARVEST]")+1]
harvscen.num<-unlist(strsplit(harvscen.num,split=" ")) #data manip so I can change harvest scenario
harvscen.num<-harvscen.num[harvscen.num != ""]
harvscen<-input[which(input == "[HARVEST]")+2]
harvscen<-unlist(strsplit(harvscen,split=" ")) #data manip so I can change harvest scenario
harvscen<-harvscen[harvscen != ""]
for(s in 1:(length(SSBlevels)-1)){
 if(s==1){
  harvscen.ofl<-unlist(strsplit(harvscen,split=" "))
  harvscen.ofl[s+1]<-Fmsy
  OFL[s]<-"--"
 } else {
  harvscen.ofl[s]<-catch.median[2]
  harvscen.ofl[s+1]<-Fmsy
 }
 OFL[s+1]<-OFLfxn(fyr=fyr,direct=direct,s=s,proj.fname=proj.fname,input=input,Fmsy=Fmsy,harvscen.num=harvscen.num,harvscen.ofl=harvscen.ofl)
 harvscen.num[s+1]<-1
}
threeblock["OFL"]<-OFL
threeblock["SSB/SSBmsy"]<-round(threeblock$SSB/Bmsy,decimals)

rmarkdown::render(paste(direct,"ProjectionTables.Rmd",sep="\\"),output_file=paste0(proj.fname,"_concatch.docx"),params=list( hcr=threeblock,title=paste(proj.fname,"Constant Catch",sep=" ")))

###Longterm MSY agepro stuff
if(domsy){
  #Run agepro 
  agepro.run<- shell(paste("  agepro40  ", paste0(msy.name,".INP"), sep=""), mustWork=F, intern=T )
  MSYstuff<-MSYageprofxn(proj.fname.b=msy.name,short.proj.name=paste0(proj.fname,s),direct=direct,decimals=decimals,fmsyold=fmsyold,nyr.avg=nyr.avg,CIwant=CIwant)
  
  ##write a txt file to send to Dan
  write(paste0("FMSY<-c(\"",fmsyold,'\",\"',Fmsy,'\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"))
  write(paste0("SSBMSY<-c(\"",Bmsyold,'\"',',\"',MSYstuff$SSBMSY," (",MSYstuff$SSBMSYCI[1]," - ",MSYstuff$SSBMSYCI[2],')\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("MSY<-c(\"",msyold,'\"',',\"',MSYstuff$MSY," (",MSYstuff$MSYCI[1]," - ",MSYstuff$MSYCI[2],')\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("Recr<-c(\"",recrold,'\"',',\"',MSYstuff$Recr," (",MSYstuff$RecrCI[1]," - ",MSYstuff$RecrCI[2],')\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("FMSYpt.est<-",Fmsy), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("SSBMSYpt.est<-",as.numeric(gsub(",","",MSYstuff$SSBMSY))), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("PYear<-c(",MSYstuff$fyr,",",MSYstuff$fyr+1,",",MSYstuff$fyr+2,",",MSYstuff$fyr+3,")"), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("PCatch<-c(\"",MSYstuff$catch4yr[1],'\"',',\"',MSYstuff$catch4yr[2]," (",MSYstuff$catch4yrCI[1,2]," - ",MSYstuff$catch4yrCI[2,2],')\",','\"',MSYstuff$catch4yr[3]," (",MSYstuff$catch4yrCI[1,3]," - ",MSYstuff$catch4yrCI[2,3],')\",','\"',MSYstuff$catch4yr[4]," (",MSYstuff$catch4yrCI[1,4]," - ",MSYstuff$catch4yrCI[2,4],')\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("PSSB<-c(\"",MSYstuff$ssb4yr[1],'\"',',\"',MSYstuff$ssb4yr[2]," (",MSYstuff$ssb4yrCI[1,2]," - ",MSYstuff$ssb4yrCI[2,2],')\",','\"',MSYstuff$ssb4yr[3]," (",MSYstuff$ssb4yrCI[1,3]," - ",MSYstuff$ssb4yrCI[2,3],')\",','\"',MSYstuff$ssb4yr[4]," (",MSYstuff$ssb4yrCI[1,4]," - ",MSYstuff$ssb4yrCI[2,4],')\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
  write(paste0("PF<-c(\"",MSYstuff$fmult4yr[1]," (",MSYstuff$fmult4yrCI[1,1]," - ",MSYstuff$fmult4yrCI[2,1],'\",\"',MSYstuff$fmult4yr[2],'\",\"',MSYstuff$fmult4yr[3],'\",\"',MSYstuff$fmult4yr[4],'\")'), file=paste(direct,paste0(msy.name,".txt"),sep="\\"),append=T)
} #end if(domsy)

###End longterm MSY
