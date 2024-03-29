# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title BeEF: Best Equal-Frequency discretization
#' @description  Discretize a quantitative variable by optimizing the obtained the Normalized Mutual Information with a target qualitative variable
#'
#' @param continuousY a vector of numeric.
#' @param factorX a vector of factor.
#' @param includeNA a boolean. TRUE to include NA value as a factor level.
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a factor.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' discreteSepalLength=BeEFdiscretization.numfact(continuousY=iris$Sepal.Length,factorX=iris$Species)
#' summary(discreteSepalLength)
#'
#' @importFrom stats complete.cases
#'
#' @export
#'
BeEFdiscretization.numfact<-function(continuousY,factorX, includeNA=T, showProgress=F){
  # progress bar
  if(!showProgress){
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
  }else{
    pbo <- pboptions(type = "timer")
    on.exit(pboptions(pbo), add = TRUE)
  }
  # handle non informative var case
  if(is.not.informative.variable(continuousY)){
    message("continuousY is not an informative variable")
    return(NA)
  }
  if(is.not.informative.variable(factorX)){
    message("factorX is not an informative variable")
    return(NA)
  }
  # format
  factorX=droplevels(as.factor(factorX))
  # Algo
  N=length(na.omit(continuousY))
  nX=length(levels(droplevels(as.factor(factorX))))
  if(nX<(N^0.6)){
    nY=2:ceiling((N^0.6)/nX) # (The maximal grid size) see section 2.2.1 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/bin/NIHMS358982-supplement-Supplemental_Figures_and_Tables.pdf
  }else{
    nY=2
  }
  nbdigitsY<-max(nchar(sub('^0+','',sub('\\.','',na.omit(continuousY))))) #util: number of digits (to avoid bug of cut2)
  NMIs=lapply(nY,function(x){
    factorY<-EFdiscretization(continuousY,x,nbdigitsY)
    list(ny=x,MaxNMI=NormalizedMI(factorY, factorX, includeNA = includeNA))
  })
  NMIsDF=as.data.frame(matrix(unlist(NMIs), ncol = 2, byrow = T))
  colnames(NMIsDF)<-c("ny", "MaxNMI")
  best=NMIsDF[which.max(NMIsDF$MaxNMI),]
  return(EFdiscretization(continuousY,best$ny,nbdigitsY))
}
