# --------------------------------------------------------------------------------
# title: Linkspotter/maxNMI
# description: compute MaxNMI correlation coefficient whatever the type of couple of variable
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title Linspotter / Maximal Normalized Mutual Information (MaxNMI) function
#' @description  Calculate the MaxNMI relationship measurement
#'
#' @param x a vector of numeric or factor.
#' @param y a vector of numeric or factor.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' maxNMI(iris$Sepal.Length,iris$Sepal.Width)
#'
#' }
#'
#' @export
maxNMI<-function(x,y){
  typeOfCouple=2*is.numeric(x)+is.numeric(y)# 3->(num,num), 2->(num,fact), 1->(fact,num), 0->(fact,fact)
  typeOfCouple=factor(as.factor(typeOfCouple),levels = c("0","1","2","3"),labels =c("fact.fact","fact.num","num.fact","num.num"))
  switch(as.character(typeOfCouple),
         fact.fact={
           NormalizedMI(x,y,includeNA = F)
         },
         fact.num={
           maxNMI_numfact(y,x,includeNA = F)
         },
         num.fact={
           maxNMI_numfact(x,y,includeNA = F)
         },
         num.num={
           maxNMI_numnum(x,y,maxNbBins=100)
         }
  )
}
