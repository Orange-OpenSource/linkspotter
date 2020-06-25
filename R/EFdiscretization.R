# --------------------------------------------------------------------------------
# title: Linkspotter/EFdiscretization
# description: Discretize a quantitative variable with equal frequency binning
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2020 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#' @title EF: Equal-Frequency discretization
#' @description  Discretize a quantitative variable with equal frequency binning
#'
#' @param continuousX a vector of numeric.
#' @param nx an integer corresponding to the desired number of intervals.
#' @param nbdigitsX number of significant digits to use in constructing levels. Default is 3.
#' @return a factor.
#'
#' @examples
#' data(iris)
#' disc.Sepal.Length=EFdiscretization(iris$Sepal.Length,5)
#' summary(disc.Sepal.Length)
#'
#' @importFrom stats quantile
#'
#' @export
#'
EFdiscretization<-function(continuousX,nx,nbdigitsX=3){
  if(length(levels(as.factor(continuousX)))<=nx){
    xfact<-as.factor(continuousX)
  }else{
    breaksX=quantile(continuousX,seq(0,1,1/nx), type = 1, na.rm=T)
    isolated=c()
    while(max(table(breaksX))>1){
      isolated=c(isolated,unique(breaksX)[which(table(breaksX)>1)])
      breaksX=quantile(continuousX[!continuousX%in%isolated],seq(0,1,1/(nx-length(isolated))), type = 1, na.rm=T)
    }
    breaksX<-c(breaksX,isolated)
    xfact=cut(continuousX,breaks = breaksX, include.lowest = T, dig.lab = nbdigitsX)
  }
  return(xfact)
}
