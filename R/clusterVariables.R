# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title Variable clustering (using Normal Mixture Modeling for Model-Based Clustering : mclust)
#' @description  Computation of a variable clustering on a correlation matrix.
#'
#' @param corMatrix a dataframe corresponding to a correlation matrix
#' @param nbCluster an integer or a vector of integers corresponding to the preferred number of cluster for the unsupervised learning.
#' @return a dataframe: the first column contains the variable names, the second column the index of the cluster they are affected to.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' corDF <- multiBivariateCorrelation(dataset = iris, corMethods = "MaxNMI")
#' # tranform to correlation matrix
#' corMatrix <- corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' # perform the clustering
#' corGroups <- clusterVariables(corMatrix = corMatrix, nbCluster = 3)
#' print(corGroups)
#'
#' @importFrom mclust Mclust mclustBIC
#'
#' @export
#'
clusterVariables<-function(corMatrix, nbCluster=1:9){

  # as NAs are not supported
  withNa=unlist(lapply(corMatrix,function(x){sum(!is.na(x))<=1}))
  nullvars=names(corMatrix)[withNa]
  corMatrix=corMatrix[!withNa,!withNa]

  #clustering
  groups=mclust::Mclust(abs(corMatrix), G = nbCluster, verbose = F)$classification # choice: Gaussian Mixture Modelling for Model-Based Clustering

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
