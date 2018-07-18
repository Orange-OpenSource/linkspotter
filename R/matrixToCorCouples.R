# --------------------------------------------------------------------------------
# title: Linkspotter/matrixToCorCouples
# description:  transform a correlation matrix into a 2 column correlation dataframe
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
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
#' @import dplyr
#' @import tidyr
#'
#' @export
matrixToCorCouples<-function(matrix, coefName="Coef.", sortByDescAbs=F){
  matrix<-as.data.frame(matrix)
  vars<-rownames(matrix)
  resfull<-matrix %>% mutate_(X1=quote(vars)) %>% gather_(key_col = 'X2', value_col = coefName, gather_cols=vars)
  res<-resfull[paste0(resfull$X1,resfull$X2)%in%apply(combn(vars,m = 2),2,paste,collapse=""),]
  if(sortByDescAbs) res<-res %>% arrange(desc(abs(eval(parse(text=coefName)))))
  res
}


