#' biomass based control rule
#' 
#' apply a hockey stick control rule
#' @param FracBmsyThreshLo is the fraction of Bmsy at which no fishing occurs.
#' @param FracBmsyThreshHi is the fraction of Bmsy above which fishing is held constant at FracFtarg.  F changes linearly between FracBmsyThreshLo and FracBmsyThreshHi
#' @param FracFtarg is the proportion of Fmsy that defines the maximum F desired.  Enter the fraction between 0 and 1, not the desired F rate.
#' @param Fmsy is exactly that; Fmsy.
#' @param CtrlRule is a place holder for later when more control rule options are added.  Now defaults to 1, biomass based.
#' @keywords AgePro
#' @export
#' @examples
#' BB_CR()
BB_CR<-function(SSBstatus=NULL,SSBthresh=NULL,SSBthreshlo=NULL,FracFtarg=NULL,Fmsy=NULL,CtrlRule=NULL) {
  if (CtrlRule==1) {      #biomass based
    ifelse (SSBstatus>=SSBthresh,Ftarg<-Fmsy*FracFtarg,Ftarg<-(Fmsy*FracFtarg)*((SSBstatus-SSBthreshlo)/(SSBthresh-SSBthreshlo))) #set target fully selected F based on a control rule (See Katsukawa 2004 Fisheries Science)
    if (Ftarg<0) Ftarg<-0  #when SSBstatus is less than SSBthreshlo then Ftarg is negative based on control rule.  This fixes that problem.
    return<-c(Ftarg)
  }
}



#' analyzes age pro output
#' 
#' summary stats from agepro
#' @param proj.fname.b is the age pro input file without the .INP at the end
#' @param direct is the directory location of proj.fname.b
#' @param fmsy is fmsy
#' @param ssbmsy is ssbmsy
#' @param decimals is the number of decimals for output values
#' @keywords AgePro
#' @export
#' @examples
#' AnaAgePro()
AnaAgePro<-function(proj.fname.b=NULL,direct=NULL,fmsy=NULL,ssbmsy=NULL,decimals=NULL,fyr=NULL){
 
  proj.fname.b<-proj.fname.b  
  direct.b<-direct
  #SSB and P(overfished)
  ssb.b <- read.table(paste(direct.b,paste(proj.fname.b,'xx3',sep='.'),sep="\\"))
  colnames(ssb.b) <- seq(fyr,fyr+ncol(ssb.b)-1)
  ssb.median.b <- apply(ssb.b,2,median)
  ssb.CI.b <- apply(ssb.b,2,function(x){quantile(x,c(0.1,0.9)) })
  frac_ofssbmsy.b<-(ssb.b[names(ssb.b) %in% (fyr:(fyr+ncol(ssb.b)-1))])/ssbmsy
  num_overfished.b<-ifelse(frac_ofssbmsy.b<0.5,1,0)
  prob_overfished.b<-colSums(num_overfished.b)/nrow(num_overfished.b)
  
  #Catch
  catch.b <- read.table(paste(direct.b,paste(proj.fname.b,'xx6',sep='.'),sep="\\"))
  colnames(catch.b) <- names(ssb.b)
  catch.median.b <- apply(catch.b,2,median)
  catch.CI.b <- apply(catch.b,2,function(x){quantile(x,c(0.1,0.9)) })
  
  #Fmult and P(overfishing)
  fmult.b <- read.table(paste(direct.b,paste(proj.fname.b,'xx9',sep='.'),sep="\\"))
  colnames(fmult.b) <- names(ssb.b)
  fmult.median.b <- apply(fmult.b,2,median)
  fmult.CI.b <- apply(fmult.b,2,function(x){quantile(x,c(0.1,0.9)) })
  frac_offmsy.b<-(fmult.b[names(fmult.b) %in% (fyr:(fyr+ncol(ssb.b)-1))])/fmsy
  num_overfishing.b<-ifelse(frac_offmsy.b>1,1,0)
  prob_overfishing.b<-colSums(num_overfishing.b)/nrow(num_overfishing.b)

resultswant<-data.frame(round(catch.median.b,digits=0),round(fmult.median.b,decimals),round(ssb.median.b,0),round(prob_overfishing.b,decimals),round(prob_overfished.b,decimals))
names(resultswant)<-c("Catch","F","SSB","P(overfishing)","P(overfished)")
return(resultswant)

} #end function


#' calculates OFL based on Fmsy fishing rate
#' 
#' OFL from agepro run
#' @param proj.fname.b is the age pro input file without the .INP at the end
#' @param direct is the directory location of proj.fname.b
#' @param fmsy is fmsy
#' @keywords AgePro
#' @export
#' @examples
#' OFLfxn()
OFLfxn<-function(fyr=NULL,direct=NULL,s=NULL,proj.fname=NULL,input=NULL,Fmsy=NULL,harvscen.num=NULL,harvscen.ofl=NULL){
  ifile <- paste(proj.fname,"OFL",s,".INP", sep="") #name of new agepro
  write(input[1:(which(input == "[HARVEST]"))], file=paste(direct,ifile,sep="\\")) #first lines of agepro unchanged
  write(harvscen.num,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.num))
  harvscen.ofl<-paste(harvscen.ofl,sep=" ",collapse=" ") #take multiple characters and make one string sep'd by a space; needed for agepro to read correctly
  write(harvscen.ofl,file=paste(direct,ifile,sep="\\"),append=T,ncolumns=length(harvscen.ofl))
  write(input[(which(input == "[HARVEST]")+3):length(input)],file=paste(direct,ifile,sep="\\"),append=T)
  ###Run AgePro with proj.fname_s.INP
  agepro.run<- shell(paste("  agepro40  ", ifile, sep=""), mustWork=F, intern=T )
  catch <- read.table(paste(direct,paste(paste0(proj.fname,"OFL",s),'xx6',sep='.'),sep="\\"))
  colnames(catch) <- seq(fyr,fyr+ncol(catch)-1)
  catch.median <- apply(catch,2,median)
  catch.median<-round(catch.median,digits=0)
  return(catch.median[s+1])
} #end OFL function


#' calculates MSY ref points from longterm agepro run
#' 
#' MSY from agepro run
#' @param proj.fname.b is the age pro input file without the .INP at the end
#' @param short.proj.name is define by code
#' @param decimals is the number decimals for output values
#' @param fmsyold is the old value for Fmsy to be replaced by the results of this projection
#' @param nyr.avg is the number of years from the end of the longterm projection to be averaged to define ref points (e.g., Bmsy)
#' @param CIwant upper and lower quantiles to define confidence intervals
#' @param direct is the directory location of proj.fname.b
#' @param fmsy is fmsy
#' @keywords AgePro
#' @export
#' @examples
#' MSYageprofxn()
MSYageprofxn<-function(proj.fname.b=NULL,short.proj.name=NULL,direct=NULL,decimals=NULL,fmsyold=NULL,nyr.avg=NULL,CIwant=NULL){
  input <- readLines(con=(paste(direct,paste(proj.fname.b,'INP',sep='.'),sep="\\"))) #read starting agepro file
  fyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],1,4)) #ID first year from Input file
  lyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],7,10)) #ID first year from Input file
  proj.yrs <- seq(fyr,lyr)
  avg.yrs <- tail(as.character(proj.yrs), nyr.avg)
  four.yrs<-head(as.character(proj.yrs),4)
  
  #SSB
  ssb.b <- read.table(paste(direct,paste(proj.fname.b,'xx3',sep='.'),sep="\\"))
  colnames(ssb.b) <- proj.yrs
  ssb.median.b <- apply(ssb.b,2,median)
  ssb.CI.b <- apply(ssb.b,2,function(x){quantile(x,c(CIwant$low,CIwant$hi)) })

  ssb.brp <- format(round(mean(ssb.median.b[avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  ssb.brp.CI <- format(round(rowMeans(ssb.CI.b[,avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  
  ssb.short <- read.table(paste(direct,paste(short.proj.name,'xx3',sep='.'),sep="\\"))
  colnames(ssb.short)<-seq(fyr,fyr+3)
  ssb.median.short <- apply(ssb.short,2,median)
  ssb.CI.short <- apply(ssb.short,2,function(x){quantile(x,c(CIwant$low,CIwant$hi)) })
  ssb4yr<-format(round(ssb.median.short[four.yrs],digits=0),big.mark=",",trim=TRUE)
  ssb4yr.CI<-format(round(ssb.CI.short[,four.yrs],digits=0),big.mark=",",trim=TRUE)
  
  #catch - MSY
  catch.b <- read.table(paste(direct,paste(proj.fname.b,'xx6',sep='.'),sep="\\"))
  colnames(catch.b) <- proj.yrs
  catch.median.b <- apply(catch.b,2,median)
  catch.CI.b <- apply(catch.b,2,function(x){quantile(x,c(CIwant$low,CIwant$hi)) })
  
  catch.brp <- format(round(mean(catch.median.b[avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  catch.brp.CI <- format(round(rowMeans(catch.CI.b[,avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  
  catch.short <- read.table(paste(direct,paste(short.proj.name,'xx6',sep='.'),sep="\\"))
  colnames(catch.short)<-seq(fyr,fyr+3)
  catch.median.short <- apply(catch.short,2,median)
  catch.CI.short <- apply(catch.short,2,function(x){quantile(x,c(CIwant$low,CIwant$hi)) })
  catch4yr<-format(round(catch.median.short[four.yrs],digits=0),big.mark=",",trim=TRUE)
  catch4yr.CI<-format(round(catch.CI.short[,four.yrs],digits=0),big.mark=",",trim=TRUE)
  
  #recruitment
  rec.b <- read.table(paste(direct,paste(proj.fname.b,'xx2',sep='.'),sep="\\"))
  colnames(rec.b) <- proj.yrs
  rec.median.b <- apply(rec.b,2,median)
  rec.CI.b <- apply(rec.b,2,function(x){quantile(x,c(CIwant$low,CIwant$hi)) })
  
  rec.brp <- format(round(mean(rec.median.b[avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  rec.brp.CI <- format(round(rowMeans(rec.CI.b[,avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  
  #Fmult
  fmult <- read.table(paste(direct,paste(short.proj.name,'xx9',sep='.'),sep="\\"))
  colnames(fmult) <- seq(fyr,fyr+3)
  fmult.median <- apply(fmult,2,median)
  fmult.CI <- apply(fmult,2,function(x){quantile(x,c(CIwant$low,CIwant$hi)) })
  
  fmult4yr<-format(round(fmult.median[four.yrs],digits=decimals),big.mark=",",trim=TRUE)
  fmult4yr.CI<-format(round(fmult.CI[,four.yrs],digits=decimals),big.mark=",",trim=TRUE)
  
  return(list("SSBMSY"=ssb.brp,"SSBMSYCI"=ssb.brp.CI,"MSY"=catch.brp,"MSYCI"=catch.brp.CI,"Recr"=rec.brp,"RecrCI"=rec.brp.CI,"fyr"=fyr,"catch4yr"=catch4yr,"catch4yrCI"=catch4yr.CI,"ssb4yr"=ssb4yr,"ssb4yrCI"=ssb4yr.CI,"fmult4yr"=fmult4yr,"fmult4yrCI"=fmult4yr.CI))
}

#' calculates Bmsy from longterm agepro run
#' 
#' Bmsy from agepro run
#' @param proj.fname.b is the age pro input file without the .INP at the end
#' @param decimals is the number decimals for output values
#' @param nyr.avg is the number of years from the end of the longterm projection to be averaged to define ref points (e.g., Bmsy)
#' @param direct is the directory location of proj.fname.b
#' @keywords AgePro
#' @export
#' @examples
#' Bmsyfxn()
Bmsyfxn<-function(proj.fname.b=NULL,direct=NULL,decimals=NULL,nyr.avg=NULL){
  input <- readLines(con=(paste(direct,paste(proj.fname.b,'INP',sep='.'),sep="\\"))) #read starting agepro file
  fyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],1,4)) #ID first year from Input file
  lyr<-as.integer(substr(input[which(input == "[GENERAL]")+1],7,10)) #ID first year from Input file
  proj.yrs <- seq(fyr,lyr)
  avg.yrs <- tail(as.character(proj.yrs), nyr.avg)
  four.yrs<-head(as.character(proj.yrs),4)
  
  #SSB
  ssb.b <- read.table(paste(direct,paste(proj.fname.b,'xx3',sep='.'),sep="\\"))
  colnames(ssb.b) <- proj.yrs
  ssb.median.b <- apply(ssb.b,2,median)
  
  ssb.brp <- format(round(mean(ssb.median.b[avg.yrs]),digits = 0),big.mark=",",trim=TRUE)
  
  return("SSBMSY"=ssb.brp)
}

