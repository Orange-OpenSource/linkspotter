# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title BeEF: Best Equal-Frequency discretization (for a couple of quantitative variables)
#' @description  Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
#'
#' @param continuousX a vector of numeric.
#' @param continuousY a vector of numeric.
#' @param includeNA a boolean. TRUE to include NA value as a factor level.
#' @param maxNbBins an integer corresponding to the number of bins limitation (for computation time limitation), maxNbBins=100 by default.
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a list of two factors.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' disc=BeEFdiscretization.numnum(iris$Sepal.Length,iris$Sepal.Width)
#' summary(disc$x)
#' summary(disc$y)
#'
#' @importFrom stats complete.cases
#' @importFrom pbapply pblapply pboptions
#'
#' @export
#'
BeEFdiscretization.numnum<-function(continuousX,continuousY,maxNbBins=100, includeNA=T, showProgress=F){
  # progress bar
  if(!showProgress){
    pbo <- pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }else{
    pbo <- pbapply::pboptions(type = "timer")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }
  # identify complete obs
  cc=complete.cases(data.frame(continuousX,continuousY))
  N=sum(cc)

  # handle non informative var case
  if(is.not.informative.variable(continuousY)){
    message("continuousY is not an informative variable")
    return(NA)
  }
  if(is.not.informative.variable(continuousY)){
    message("continuousY is not an informative variable")
    return(NA)
  }
  # util: number of digits (to avoid bug of cut2)
  nbdigitsX<-max(nchar(sub('^0+','',sub('\\.','', na.omit(continuousX)))))
  nbdigitsY<-max(nchar(sub('^0+','',sub('\\.','', na.omit(continuousY)))))
  # threshold
  threshold=min(c((N^0.6),maxNbBins,length(unique(continuousX))*length(unique(continuousY))),na.rm=T)
  if(threshold<4) threshold<-4 #(2*2)
  # Algo
  eg=expand.grid(2:ceiling(threshold/2),2:ceiling(threshold/2))
  egt=eg[,1]*eg[,2]
  eg2=eg[egt<=threshold,]
  NMIs = pbapply::pblapply(as.data.frame(t(eg2)), function(n) {
    xfact<-EFdiscretization(continuousX,n[1],nbdigitsX)
    yfact<-EFdiscretization(continuousY,n[2],nbdigitsY)
    nmi = NormalizedMI(xfact, yfact, includeNA = includeNA)
    list(nx = n[1], ny = n[2], NMI = nmi)
  })
  NMIsDF = as.data.frame(matrix(unlist(NMIs), ncol = 3, byrow = T))
  colnames(NMIsDF) <- c("nx", "ny", "MaxNMI")
  best = NMIsDF[which.max(NMIsDF$MaxNMI), ]
  b_xfact<-EFdiscretization(continuousX,best$nx,nbdigitsX)
  b_yfact<-EFdiscretization(continuousY,best$ny,nbdigitsY)
  return(list(x = b_xfact, y = b_yfact))
}
