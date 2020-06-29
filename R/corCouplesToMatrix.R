# --------------------------------------------------------------------------------
# title: Linkspotter/corCouplesToMatrix
# description: transform a 2 column correlation dataframe into a correlation matrix
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#
#' @title Couples to matrix
#' @description  Transform a 2 column correlation dataframe into a correlation matrix
#'
#' @param x1_x2_val a specific dataframe containing correlations values resulting from the function multiBivariateCorrelation() and containing only one coeffecient type.
#' @return a dataframe corresponding to a correlation matrix.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' corDF<-multiBivariateCorrelation(dataset = iris, corMethods = "MaxNMI")
#' corMatrix<-corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' print(corMatrix)
#' corCouples<-matrixToCorCouples(corMatrix,coefName="pearson")
#' print(corCouples)
#'
#' @export
corCouplesToMatrix<-function(x1_x2_val){
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
# corCouplesToMatrix<-function(corCouples){
#   cnames=colnames(corCouples)
#   cc_1<-cc %>% transmute_(X1=as.character(cnames[1]),X2=as.character(cnames[2]),coef=cnames[3])
#   cc_2<-cc %>% mutate_(temp='X1') %>% transmute_(X1='X2',X2='temp',coef=cnames[3])
#   vars<-cc_1 %>% bind_rows(cc_2) %>% select_('X1') %>% distinct()
#   filling<-vars %>% mutate_('X2'='X1',coef=rep(1,nrow(vars)))
#   ccc<-cc_1 %>% bind_rows(cc_2, filling) %>% spread(key = "X2",value = "coef")
#   ccc %>% 'row.names<-'(ccc$X1) %>% select(-1)
# } # quelques warnings
