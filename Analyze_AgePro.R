#control rule function
#########################################
BB_CR<-function(SSBstatus=NULL,SSBthresh=NULL,SSBthreshlo=NULL,FracFtarg=NULL,Fmsy=NULL,CtrlRule=NULL) {
  if (CtrlRule==1) {      #biomass based
    ifelse (SSBstatus>=SSBthresh,Ftarg<-Fmsy*FracFtarg,Ftarg<-(Fmsy*FracFtarg)*((SSBstatus-SSBthreshlo)/(SSBthresh-SSBthreshlo))) #set target fully selected F based on a control rule (See Katsukawa 2004 Fisheries Science)
    if (Ftarg<0) Ftarg<-0  #when SSBstatus is less than SSBthreshlo then Ftarg is negative based on control rule.  This fixes that problem.
    return<-c(Ftarg)
  }
}
############################################



AnaAgePro<-function(proj.fname.b=NULL,direct=NULL,fmsy=NULL,ssbmsy=NULL){

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

resultswant<-data.frame(round(catch.median.b,digits=0),round(fmult.median.b,2),round(ssb.median.b,0),round(prob_overfishing.b,2),round(prob_overfished.b,2))
names(resultswant)<-c("Catch","F(ages 7-8)","SSB","P(overfishing)","P(overfished)")
return(resultswant)

} #end function

