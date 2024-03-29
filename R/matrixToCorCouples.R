# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title Matrix to couples
#' @description  Transform a correlation matrix into a correlation couples dataframe
#'
#' @param matrix a dataframe corresponding to a matrix of correlation.
#' @param coefName a string: the name of the coefficient the values of the matrix represent.
#' @param sortByDescAbs a boolean to decide if to sort by descending absolute value of the coefficient.
#' @return a dataframe corresponding to all correlation couples from the matrix.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' corDF<-multiBivariateCorrelation(dataset = iris)
#' corMatrix<-corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"pearson")])
#' print(corMatrix)
#' corCouples<-matrixToCorCouples(matrix = corMatrix,coefName="pearson")
#' print(corCouples)
#'
#' @importFrom dplyr mutate arrange desc
#' @importFrom tidyr pivot_longer
#'
#' @export
matrixToCorCouples<-function(matrix, coefName="Coef.", sortByDescAbs=F){
  matrix<-as.data.frame(matrix)
  vars<-rownames(matrix)
  resfull<-matrix %>%
    mutate(X1=vars) %>%
    pivot_longer(cols=vars, names_to = "X2", values_to = coefName)
  res<-resfull[paste0(resfull$X1,resfull$X2)%in%apply(combn(vars,m = 2),2,paste,collapse=""),]
  if(sortByDescAbs){
    res<-res %>%
      arrange(desc(abs(eval(parse(text=coefName)))))
  }
  res
}
