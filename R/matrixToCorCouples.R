# --------------------------------------------------------------------------------
# title: Linkspotter/matrixToCOrCouples
# description:  transform a correlation matrix into a 2 column correlation dataframe
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#
#' @title Matrix to couples
#' @description  Transform a correlation matrix into a correlation couples dataframe
#'
#' @param matrix a dataframe corresponding to a matrix of correlation.
#' @param coefName a string: the name of the coefficient the values of the matrix represent.
#' @return a dataframe corresponding to a correlation matrix.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' corDF<-multiBivariateCorrelation(mixedData = iris, corMethods = "MaxNMI")
#' corMatrix<-corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' print(corMatrix)
#' corCouples<-matrixToCOrCouples(corMatrix,coefName="pearson")
#' print(corCouples)
#' }
#'
#' @import dplyr
#' @import tidyr
#' @export
matrixToCorCouples<-function(matrix,coefName){
  vars<-rownames(matrix)
  resfull<-matrix %>% mutate_(X1=quote(vars)) %>% gather_(key = 'X2', value = coefName,select_vars_(names(.),names(.),exclude="X1"))
  resfull[paste0(resfull$X1,resfull$X2)%in%apply(combn(vars,m = 2),2,paste,collapse=""),]
}


