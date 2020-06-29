# --------------------------------------------------------------------------------
# title: Linkspotter/is_not_informative_variable
# description: Computes the MaxNMI between the two variables whatever their types, by discretizing using Best Equal-Frequency-based discretization (BeEF) if necessary.
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
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
