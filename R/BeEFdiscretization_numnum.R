# --------------------------------------------------------------------------------
# title: Linkspotter/BeEFdiscretization.numnum
# description: Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#' @title BeEF: Best Equal-Frequency discretization (for a couple of quantitative variables)
#' @description  Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
#'
#' @param continuousX a vector of numeric.
#' @param continuousY a vector of numeric.
#' @param maxNbBins an integer corresponding to the number of bins limitation (for computation time limitation), maxNbBins=100 by default.
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' disc=BeEFdiscretization.numnum(iris$Sepal.Length,iris$Sepal.Width)
#' summary(disc$x)
#' summary(disc$y)
#'
#' @importFrom Hmisc cut2
#' @importFrom stats complete.cases
#' @importFrom pbapply pblapply pboptions
#'
#' @export
#'
BeEFdiscretization.numnum<-function(continuousX,continuousY,maxNbBins=100,showProgress=F){

  #progress bar
  if(!showProgress){
    pbo <- pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }else{
    pbo <- pbapply::pboptions(type = "timer")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }

  # Only on complete obs
  cc=complete.cases(data.frame(continuousX,continuousY))
  continuousX=continuousX[cc]
  continuousY=continuousY[cc]

  N=sum(cc)

  #if no variability
  if((length(levels(droplevels(as.factor(continuousX))))<2)|(length(levels(droplevels(as.factor(continuousY))))<2))
    return(list(nx=NA,ny=NA,MaxNMI=NA))

  #util: number of digits (to avoid bug of cut2)
  nbdigitsX<-max(nchar(sub('^0+','',sub('\\.','',continuousX))))
  nbdigitsY<-max(nchar(sub('^0+','',sub('\\.','',continuousY))))

  #threshold
  threshold=min(c((N^0.6),maxNbBins,length(unique(continuousX))*length(unique(continuousY))),na.rm=T)
  if(threshold<4) threshold<-4 #(2*2)
  # Algo
  eg=expand.grid(2:ceiling(threshold/2),2:ceiling(threshold/2))
  egt=eg[,1]*eg[,2]
  eg2=eg[egt<=threshold,]
  NMIs=pbapply::pblapply(as.data.frame(t(eg2)),function(n){
    breaksX=unique(quantile(continuousX,seq(0,1,1/n[1]), type = 1, na.rm=T))
    while(length(breaksX)<3){
      n[1]<-n[1]+1
      breaksX=unique(quantile(continuousX,seq(0,1,1/n[1]), type = 1, na.rm=T))
    }
    xfact=cut(continuousX,breaks = breaksX, include.lowest = T, dig.lab = nbdigitsX)
    breaksY=unique(quantile(continuousY,seq(0,1,1/n[2]), type = 1, na.rm=T))
    while(length(breaksY)<3){
      n[2]<-n[2]+1
      breaksY=unique(quantile(continuousY,seq(0,1,1/n[2]), type = 1, na.rm=T))
    }
    yfact=cut(continuousY,breaks = breaksY, include.lowest = T, dig.lab = nbdigitsY)
    nmi=NormalizedMI(xfact,yfact);
    list(nx=n[1],ny=n[2],NMI=nmi)
  })
  NMIsDF=as.data.frame(matrix(unlist(NMIs),ncol = 3, byrow = T))
  colnames(NMIsDF)<-c("nx","ny","MaxNMI")
  best=NMIsDF[which.max(NMIsDF$MaxNMI),]
  return(list(x=Hmisc::cut2(continuousX,g = best$nx, digits = nbdigitsX),y=Hmisc::cut2(continuousY,g = best$ny, digits = nbdigitsY)))
}
