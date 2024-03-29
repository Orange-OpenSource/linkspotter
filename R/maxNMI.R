# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title Maximal Normalized Mutual Information (MaxNMI)
#' @description Computes the MaxNMI between the two variables whatever their types, by discretizing using Best Equal-Frequency-based discretization (BeEF) if necessary.
#'
#' @param x a vector of numeric or factor.
#' @param y a vector of numeric or factor.
#' @param includeNA a boolean. TRUE to include NA value as a factor level.
#' @param maxNbBins an integer corresponding to the number of bins limitation (for computation time limitation), maxNbBins=100 by default.
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' maxNMI(iris$Sepal.Length,iris$Species)
#' maxNMI(iris$Sepal.Length,iris$Sepal.Width)
#'
#' @export
maxNMI<-function(x, y, includeNA=T, maxNbBins=100, showProgress = F){
  # handle non informative var case
  if(is.not.informative.variable(x)){
    message("x is not an informative variable")
    return(NA)
  }
  if(is.not.informative.variable(y)){
    message("y is not an informative variable")
    return(NA)
  }
  # determine the type of couple
  typeOfCouple=2*is.numeric(x)+is.numeric(y)# 3->(num,num), 2->(num,fact), 1->(fact,num), 0->(fact,fact)
  typeOfCouple=factor(as.factor(typeOfCouple),levels = c("0","1","2","3"),labels =c("fact.fact","fact.num","num.fact","num.num"))
  # perform the needed computation according to the type of couple
  switch(as.character(typeOfCouple),
         fact.fact={
           NormalizedMI(x,y,includeNA = includeNA)
         },
         fact.num={
           yfact=BeEFdiscretization.numfact(y,x,includeNA = includeNA, showProgress = showProgress)
           NormalizedMI(x,yfact,includeNA = includeNA)
         },
         num.fact={
           xfact=BeEFdiscretization.numfact(x,y,includeNA = includeNA, showProgress = showProgress)
           NormalizedMI(xfact,y,includeNA = includeNA)
         },
         num.num={
           disc=BeEFdiscretization.numnum(x,y,maxNbBins=maxNbBins, showProgress = showProgress)
           NormalizedMI(disc$x,disc$y,includeNA = includeNA)
         }
  )
}
