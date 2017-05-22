# --------------------------------------------------------------------------------
# title: Linkspotter/BeEFdiscretization.numnum
# description: Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title BeEF: Best Equal-Frequency discretization (for a couple of quantitative variables)
#' @description  Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
#'
#' @param continuousX a vector of numeric.
#' @param continuousY a vector of numeric.
#' @param maxNbBins an integer corresponding to the number of bin limitation, maxNbBins=100 by default.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' disc=BeEFdiscretization.numnum(iris$Sepal.Length,iris$Sepal.Width)
#' summary(disc$x)
#' summary(disc$y)
#'
#' }
#'
#' @export
#'
#' @importFrom Hmisc cut2
#' @import stats
BeEFdiscretization.numnum<-function(continuousX,continuousY,maxNbBins=NA){

  # Only on complete obs
  cc=complete.cases(data.frame(continuousX,continuousY))
  continuousX=continuousX[cc]
  continuousY=continuousY[cc]

  N=sum(cc)

  #if no variability
  if((length(levels(droplevels(as.factor(continuousX))))<2)|(length(levels(droplevels(as.factor(continuousY))))<2))
    return(list(nx=NA,ny=NA,MaxNMI=NA))

  #threshold
  threshold=min(c((N^0.6),maxNbBins,length(unique(continuousX))*length(unique(continuousY))),na.rm=T)
  if(threshold<4) threshold<-4 #(2*2)
  # Algo
  eg=expand.grid(2:ceiling(threshold/2),2:ceiling(threshold/2))
  egt=eg[,1]*eg[,2]
  eg2=eg[egt<=threshold,]
  NMIs=lapply(as.data.frame(t(eg2)),function(n){
    #xfact=cut(continuousX,breaks = unique(quantile(continuousX,probs = seq(0,1,1/n[1]),type = 1)), include.lowest = T);
    #yfact=cut(continuousY,breaks = unique(quantile(continuousY,probs = seq(0,1,1/n[2]),type = 1)), include.lowest = T);
    xfact=Hmisc::cut2(continuousX,g = n[1]);
    yfact=Hmisc::cut2(continuousY,g = n[2]);
    nmi=NormalizedMI(xfact,yfact);
    list(nx=n[1],ny=n[2],NMI=nmi)
  })
  NMIsDF=as.data.frame(matrix(unlist(NMIs),ncol = 3, byrow = T))
  colnames(NMIsDF)<-c("nx","ny","MaxNMI")
  best=NMIsDF[which.max(NMIsDF$MaxNMI),]
  return(list(x=Hmisc::cut2(continuousX,g = best$nx),y=Hmisc::cut2(continuousY,g = best$ny)))
}
