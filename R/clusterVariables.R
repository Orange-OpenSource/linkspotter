# --------------------------------------------------------------------------------
# title: Linkspotter/clusterVariables
# description: clustering variables (using Normal Mixture Modeling for Model-Based Clustering : mclust)
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title Variable clustering (using Normal Mixture Modeling for Model-Based Clustering : mclust)
#' @description  Computation of a variable clustering on a correlation matrix.
#'
#' @param correlationMatrix a dataframe corresponding to a correlation matrix
#' @param nbCluster an intyeger or a vector of integers corresponding to the prefered number of cluster for the unsupervised learning.
#' @return a dataframe: the first column contains the variable names, the second column the index of the cluster they are affected to.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' corDF=multiBivariateCorrelation(mixedData = iris, corMethods = "MaxNMI")
#' # tranform to correlation matrix
#' corMatrix=matrixOfValuesOfAllCouples(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' # perform the clustering
#' corGroups=clusterVariables(correlationMatrix = corMatrix, nbCluster = 3)
#' print(corGroups)
#' }
#'
#' @export
#'
#' @importFrom mclust Mclust mclustBIC
clusterVariables<-function(correlationMatrix, nbCluster=1:9){

  # as NAs are not supported
  withNa=unlist(lapply(correlationMatrix,function(x){sum(!is.na(x))<=1}))
  nullvars=names(correlationMatrix)[withNa]
  correlationMatrix=correlationMatrix[!withNa,!withNa]

  #clustering
  groups=mclust::Mclust(abs(correlationMatrix), G = nbCluster)$classification # choice: Gaussian Mixture Modelling for Model-Based Clustering

  #format result
  res=data.frame(var=names(groups),group=groups,row.names = NULL)

  #useless variables
  if(length(nullvars)>0){
    uv=data.frame(var=nullvars, group=NA)
  }else{
    uv=NULL
  }

  #final
  data.frame(rbind(res,uv))
}
