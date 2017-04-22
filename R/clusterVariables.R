# --------------------------------------------------------------------------------
# title: Linkspotter/clusterVariables
# description: clustering variables (using Normal Mixture Modeling for Model-Based Clustering : mclust)
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
# clustering variables (using Normal Mixture Modeling for Model-Based Clustering : mclust)
clusterVariables<-function(correlation_matrix, nbCluster=1:9){

  # as NAs are not supported
  withNa=unlist(lapply(correlation_matrix,function(x){sum(!is.na(x))<=1}))
  nullvars=names(correlation_matrix)[withNa]
  correlation_matrix=correlation_matrix[!withNa,!withNa]

  #clustering
  groups=mclust::Mclust(abs(correlation_matrix), G = nbCluster)$classification # choice: Gaussian Mixture Modelling for Model-Based Clustering

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
