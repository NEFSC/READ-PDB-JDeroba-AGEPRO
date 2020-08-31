
#' Iteratively apply agepro
#' 
#' Does iterative agepro runs
#' @import rmarkdown
#' @param FracBmsyThreshLo is the fraction of SSBmsy below which no fishing occurs.  Above this value, F increases linearly to FracBmsyThreshHi.  (Default is 0.0 and produces constant F if FracBmsyThreshHi also 0.0)
#' @param FracBmsyThreshHi is the fraction of SSBmsy above which fishing is held constant at FracFtarg.  Below this value, F declines linearly to FracBmsyThreshLo.  (Default is 0.0 and produces constant F if FracBmsyThreshLo also 0.0)
#' @param FracFtarg is the proportion of Fmsy that defines the maximum F desired.  Enter the fraction between 0 and 1, not the desired F rate.
#' @param Fmsy is exactly that; Fmsy.
#' @param SSBmsy is spawning stock biomass at msy level
#' @param direct is the directory location of a functional agepro .INP file; proj.fname
#' @param proj.fname is a functional age pro input file without the .INP at the end.  First year should be setup as "bridge year" with catch specified.
#' @param decimals is the number of decimals for output values
#' @param domsy do longterm MSY ref point run: TRUE or FALSE.  If TRUE, creates LaTex code for groundfish short report.
#' @param msy.name needed only if domsy=TRUE. name of functional agepro run for longterm MSY ref point projection
#' @param fmsyold needed only if domsy=TRUE. old fmsy value to compare to new projection
#' @param SSBmsyold needed only if domsy=TRUE. is old SSBmsy value to compare to new projection
#' @param msyold needed only if domsy=TRUE. is old msy value to compare to new projection
#' @param recrold needed only if domsy=TRUE. is old recruitment at MSY value to compare to new projection
#' @param nyr.avg needed only if domsy=TRUE. is the number of years from the end of the longterm projection (e.g., last 10 years) to be averaged to define ref points (e.g., Bmsy)
#' @keywords AgePro
#' @export
#' @examples
#' AgeProRun()
AgeProRun<-function(direct="missing",proj.fname="missing",FracBmsyThreshHi=0.0,FracBmsyThreshLo=0.0,FracFtarg=1.0,SSBmsy="missing",Fmsy="missing",decimals=3,domsy=FALSE,
                    msy.name="missing",CIwantLow=0.05,CIwantHi=0.95,fmsyold=999,SSBmsyold=999,msyold=999,recrold=999,nyr.avg=10){
  
  inputsa<-c(direct,proj.fname,FracBmsyThreshHi,FracBmsyThreshLo,FracFtarg,Fmsy,decimals)
  inputnames<-c("direct","proj.fname","FracBmsyThreshHi","FracBmsyThreshLo","FracFtarg","Fmsy","decimals")
  for(c in 1:length(inputnames)){
    if(inputsa[c]=="missing"){stop(paste0(inputnames[c]," is missing"))}
  }
  if(domsy==TRUE & msy.name=="missing") {stop(paste0("msy.name"," is missing"))}
  if(domsy==FALSE & SSBmsy=="missing") {stop(paste0("SSBmsy"," is missing"))}
  
  
direct<-direct
setwd(direct)
proj.fname<-proj.fname #AgePro name to work from
file.copy(system.file("exe","agepro40.exe",package="AgePro"),direct,overwrite = T)

#input control rule parameters and MSY reference points
FracBmsyThreshHi=c(FracBmsyThreshHi) # For control rule; Threshold as fraction of Bmsy to switch from Fmsy*FracFtarg as target F to linear decline to zero
FracBmsyThreshLo=c(FracBmsyThreshLo) #For control rule; Level of SSB as fraction of Bmsy where target F set to 0
FracFtarg<-c(FracFtarg) #fraction of Fmsy that serves as max target F in control rule
if(domsy==FALSE) {Bmsy<-SSBmsy}
Fmsy<-Fmsy
decimals<-decimals

###inputs for long-term MSY agepro run; if desired
domsy<-domsy #Do the MSY, TRUE, or not, FALSE
msy.name<-msy.name #name of agepro
CIwant<-data.frame(low=CIwantLow,hi=CIwantHi) #What CIs do you want?
fmsyold<-fmsyold
Bmsyold<-SSBmsyold
msyold<-msyold
recrold<-recrold
nyr.avg<-nyr.avg #average how many of the last years of long-term proj for ref point?

################################################################
###Longterm BMSY agepro stuff
if(domsy){
  #Run agepro 
  agepro.run<- shell(paste("  agepro40  ", paste0(msy.name,".INP"), sep=""), mustWork=F, intern=T )
  Bmsy<-Bmsyfxn(proj.fname.b=msy.name,direct=direct,decimals=decimals,nyr.avg=nyr.avg)
} #end if(domsy)
###End longterm BMSY

###Short-term projections and control rule application
#Read in an agepro input for manipulation later
input <- readLines(con=(paste(direct,paste(proj.fname,'INP',sep='.'),sep="\\"))) #read starting agepro file
fyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],1,4)) #ID first year from Input file

#extract the element of input file where F or catch values specified
harvscen.num<-input[which(input == "[HARVEST]")+1]
harvscen.num<-unlist(strsplit(harvscen.num,split=" ")) #data manip so I can change harvest scenario
harvscen.num<-harvscen.num[harvscen.num != ""]
harvscen.num<-c("1",rep("0",length(harvscen.num)-1))
harvscen<-input[which(input == "[HARVEST]")+2]
harvscen<-unlist(strsplit(harvscen,split=" ")) #data manip so I can change harvest scenario
harvscen<-harvscen[harvscen != ""]
harvscen[2:length(harvscen)]<-Fmsy  #Will be replaced in loops below
#Create OFL holder
OFL<-c()

ifile <- paste(proj.fname,0,".INP", sep="") #name of new agepro
ifile_noext<-paste(proj.fname,0, sep="") #need this with no extension for other calls
write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)

#Run agepro using intial input file; just a starting point.
agepro.run<- shell(paste("  agepro40  ", ifile , sep=""), mustWork=F, intern=T )

#Need status in year one of projection to apply cr in loop below
ssb <- read.table(paste(direct,paste(ifile_noext,'xx3',sep='.'),sep="\\"))
colnames(ssb) <- seq(fyr,fyr+ncol(ssb)-1)
ssb.median <- apply(ssb,2,median)
SSBlevels<-ssb.median/Bmsy #relative to Bmsy
yr.names<-names(SSBlevels) #need for a function below.
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
  ifile_noext <- paste(proj.fname,s,sep="") #name of new agepro
  write(input[1:(which(input == "[HARVEST]")+1)], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
  harvscen[s+1]<-round(Fmult,decimals)
  harvscen<-paste(harvscen,sep=" ",collapse=" ") #take multiple characters and make one string sep'd by a space; needed for agepro to read correctly
  write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
  write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
  ###Run AgePro with proj.fname_s.INP
  agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
}
annual<-AnaAgePro(proj.fname.b=paste0(proj.fname,s),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy,decimals=decimals,fyr=fyr)

######Do OFL - Get NAA from final run where HCR was applied above, just calculate OFL using Baranov
######Solve for F to produce desired Canadian catch
tempxx1.ofl<-read.table(paste(direct,paste0(ifile_noext,'.xx1'),sep="\\"))
input.new.ofl<- readLines(con=(paste(direct,paste(ifile_noext,'INP',sep='.'),sep="\\"))) #read starting agepro file
for(s in 1:(length(SSBlevels)-1)){
  
  if(s==1) {
    temp.concatch<-apply.baranov(xx1=tempxx1.ofl,input=input.new.ofl,s=s,yr.names=yr.names,nages=nages,fmult=Fmsy)
    OFL<-temp.concatch
  } else {
    temp.concatch<-apply.baranov(xx1=tempxx1.ofl,input=input.new.ofl,s=s,yr.names=yr.names,nages=nages,fmult=Fmsy)
    OFL<-data.frame(OFL,temp.concatch)
  }
}
OFL<-round(OFL,0)
OFL<-data.frame("--",OFL)
OFL<-t(OFL)
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
annual.catch<-AnaAgePro(proj.fname.b=paste0(proj.fname,s,"b"),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy,decimals=decimals,fyr=fyr)
annual["P(overfishing)"]<-annual.catch["P(overfishing)"]
annual["SSB/SSBmsy"]<-round(annual$SSB/Bmsy,decimals)

rmarkdown::render(paste(direct,"ProjectionTables.Rmd",sep="\\"),output_file=paste0(proj.fname,"_annual.docx"),params=list( hcr=annual,title=paste(proj.fname,"annual",sep=" ")))
#rmarkdown::render(system.file("rmd","ProjectionTables.Rmd",package="AgePro"),output_file=paste0(proj.fname,"_annual.docx"),output_dir=direct,params=list( hcr=annual,title=paste(proj.fname,"annual",sep=" ")))

#Run HCR with constant catch, i.e., catch in year one applies to all years
ifile <- paste(proj.fname,paste0(s,"_concatch"),".INP", sep="") #name of new agepro
ifile_noext <- paste(proj.fname,paste0(s,"_concatch"), sep="") #name of new agepro
write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
harvscen.num<-rep(1,length(catch))
write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
harvscen<-c(catch.median[1],rep(round(catch.median[2],0),(length(catch)-1)))
write(harvscen,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen))
write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
threeblock<-AnaAgePro(proj.fname.b=paste0(proj.fname,s,"_concatch"),direct=direct,fmsy=Fmsy,ssbmsy=Bmsy,decimals=decimals,fyr=fyr)

######Do OFL for constant catch
tempxx1.ofl<-read.table(paste(direct,paste0(ifile_noext,'.xx1'),sep="\\"))
input.new.ofl<- readLines(con=(paste(direct,paste(ifile_noext,'INP',sep='.'),sep="\\"))) #read starting agepro file

for(s in 1:(length(SSBlevels)-1)){
  if(s==1) {
    temp.concatch<-apply.baranov(xx1=tempxx1.ofl,input=input.new.ofl,s=s,yr.names=yr.names,nages=nages,fmult=Fmsy)
    OFL<-temp.concatch
  } else {
    temp.concatch<-apply.baranov(xx1=tempxx1.ofl,input=input.new.ofl,s=s,yr.names=yr.names,nages=nages,fmult=Fmsy)
    OFL<-data.frame(OFL,temp.concatch)
  }
}
OFL<-round(OFL,0)
OFL<-data.frame("--",OFL)
OFL<-t(OFL)
if(FALSE){
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
}
threeblock["OFL"]<-OFL
threeblock["SSB/SSBmsy"]<-round(threeblock$SSB/Bmsy,decimals)

rmarkdown::render(paste(direct,"ProjectionTables.Rmd",sep="\\"),output_file=paste0(proj.fname,"_concatch.docx"),params=list( hcr=threeblock,title=paste(proj.fname,"Constant Catch",sep=" ")))
#rmarkdown::render(system.file("rmd","ProjectionTables.Rmd",package="AgePro"),output_file=paste0(proj.fname,"_concatch.docx"),output_dir=direct,params=list( hcr=threeblock,title=paste(proj.fname,"Constant Catch",sep=" ")))

################################################################
###Longterm MSY agepro stuff
if(domsy){
  #Run agepro 
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
} #end big function
