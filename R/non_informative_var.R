# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title Is a vector an non informative variable
#' @description This function determines if a given vector of numeric or factor is a non informative variable or not.
#'
#' @param x a vector of numeric or factor.
#' @param includeNA a boolean. TRUE to include NA value as a factor level.
#'
#' @examples
#' data(iris)
#' is.not.informative.variable(iris$Sepal.Length)
#'
#' @importFrom stats var
#'
#' @export
is.not.informative.variable<-function(x, includeNA=T){
  # include NA or not
  if(includeNA){
    if(includeNA&(sum(is.na(x))>0)){
      x=as.character(x)
      x[is.na(x)] <- c("NA")
      x=as.factor(x)
    }
  }
  # boolean
  !(
  sum(!is.na(x))>0 && # not only NA values
  !(is.numeric(x)&&(var(x, na.rm=T)%in%c(0,NA))) && # not a single numeric value
  !(!is.numeric(x)&&(length(levels(factor(x)))<2)) # not a single level
  )
}
# empty vars
emptyVar<-function(dataset){
  names(dataset)[unlist(lapply(dataset,function(x){sum(!is.na(x))==0}))]
}
uselessVar<-function(dataset){
  names(dataset)[lapply(dataset,function(x){length(levels(as.factor(x)))})==1]
}
