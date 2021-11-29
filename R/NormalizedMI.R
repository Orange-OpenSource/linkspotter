# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title Maximal Normalized Mutual Information (MaxNMI) function for 2 categorical variables
#' @description  Calculate the MaxNMI relationship measurement for 2 categorical variables
#'
#' @param x a vector of factor.
#' @param y a vector of factor.
#' @param includeNA a boolean. TRUE to include NA value as a factor level.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' discreteSepalLength=BeEFdiscretization.numfact(continuousY=iris$Sepal.Length,factorX=iris$Species)
#' NormalizedMI(iris$Species,discreteSepalLength)
#'
#' @importFrom infotheo mutinformation
#' @importFrom stats complete.cases
#'
#' @export
#'
NormalizedMI<-function(x,y, includeNA=T){#-> ratio d'incertitude/entropie expliquee
  # include NA if asked
  if(includeNA){
    if(includeNA&(sum(is.na(x))>0)){
      x=as.character(x)
      x[is.na(x)] <- c("NA")
      x=as.factor(x)
    }
    if(includeNA&(sum(is.na(y))>0)){
      y=as.character(y)
      y[is.na(y)] <- c("NA")
      y=as.factor(y)
    }
  }
  # handle non informative var case
  if(is.not.informative.variable(x)){
    message("x is not an informative variable")
    return(NA)
  }
  if(is.not.informative.variable(y)){
    message("y is not an informative variable")
    return(NA)
  }
  # if includeNA=F, remove NA
  cc=complete.cases(cbind(x,y))
  if(sum(cc)==0){
    message("no value available at the same time between x and y")
    return(NA)
  }
  x2=droplevels(as.factor(x[cc]))
  y2=droplevels(as.factor(y[cc]))
  # calculate the normalized mutual information
  infotheo::mutinformation(x2,y2)/log(min(c(length(levels(x2)),length(levels(y2))))) # (normalization) see section 2.2.2 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/bin/NIHMS358982-supplement-Supplemental_Figures_and_Tables.pdf
}
