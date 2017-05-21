# --------------------------------------------------------------------------------
# title: Linkspotter/matrixOfValuesOfAllCouples
# description: transform a 2 column correlation dataframe into a correlation matrix
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#
#' @title Transform a correlation dataframe into a correlation matrix
#' @description  Transform a 2 column correlation dataframe into a correlation matrix
#'
#' @param x1_x2_val a specific dataframe containing correlations values resulting from the function multiBivariateCorrelation() and containing only one coeffecient type.
#' @return a dataframe corresponding to a correlation matrix.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' corDF=multiBivariateCorrelation(mixedData = iris, corMethods = "MaxNMI")
#' corMatrix=matrixOfValuesOfAllCouples(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' print(corMatrix)
#' }
#'
#' @export
matrixOfValuesOfAllCouples<-function(x1_x2_val){
  #conforme uniquement aux tableaux de couples complets
  #quelques conditions a ajouter

  x1_x2_val=x1_x2_val[,1:3]

  all_x=unique(c(as.character(x1_x2_val[,1]),as.character(x1_x2_val[,2])))
  colnames(x1_x2_val)<-c("x1","x2","val")
  complete_x1_x2_val<-data.frame(rbind(as.matrix(x1_x2_val[,c("x1","x2","val")]),
                                       as.matrix(x1_x2_val[,c("x2","x1","val")]),
                                       as.matrix(data.frame(x1=all_x,x2=all_x,val=rep(1,length(all_x))))
  ),
  row.names = NULL)#1:(2*nrow(rawNet$edges)+length(all_x)))
  complete_x1_x2_val$val<-as.numeric(as.character(complete_x1_x2_val$val))
  as.data.frame.matrix(with(complete_x1_x2_val[,c('x1','x2')], tapply(complete_x1_x2_val[,'val'], list(x1,x2), sum )))
}
