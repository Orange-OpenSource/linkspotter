# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title EF: Equal-Frequency discretization
#' @description  Discretize a quantitative variable with equal frequency binning if possible
#'
#' @param continuousX a vector of numeric.
#' @param nX an integer corresponding to the desired number of intervals.
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
EFdiscretization<-function(continuousX,nX,nbdigitsX=3){
  # handle non informative var case
  if(is.not.informative.variable(continuousX)){
    message("continuousX is not an informative variable")
    return(NA)
  }
  # algo
  if(length(levels(as.factor(continuousX)))<=nX){
    xfact<-as.factor(continuousX)
  }else{
    breaksX=quantile(continuousX,seq(0,1,1/nX), type = 1, na.rm=T)
    isolated=c()
    while(max(table(breaksX))>1){
      isolated=c(isolated,unique(breaksX)[which(table(breaksX)>1)])
      breaksX=quantile(continuousX[!continuousX%in%isolated],seq(0,1,1/(nX-length(isolated))), type = 1, na.rm=T)
    }
    breaksX<-c(breaksX,isolated)
    xfact=cut(continuousX,breaks = breaksX, include.lowest = T, dig.lab = nbdigitsX)
  }
  return(xfact)
}
