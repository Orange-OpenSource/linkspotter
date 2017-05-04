# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterOnFile
# description: function to load and properly proccess a file
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
# function to load and properly proccess the file
#' @import utils
linkspotterOnFile<-function(file1, header, sep, quote, corMethods=c("pearson","spearman","kendall","mic","MaxNormMutInfo"), defaultMinCor=0.3, defaultCorMethod=corMethods[length(corMethods)], clusteringCorMethod=corMethods[length(corMethods)], nbCluster=1:9, printInfo=T, appTitle="Linkspotter", ...){
  # commodity
  if (is.null(file1))
    return(NULL)
  # read file
  data=read.table(stringsAsFactors = T, file = file1$datapath, header=header, sep=sep,
                  quote=quote, ...)
  # first format
  for(i in 1:ncol(data)){
    if(is.factor(data[,i]) | (sum(is.na(data[,i]))==length(data[,i])) | is.logical(data[,i]) | length(levels(as.factor(data[,i])))<20 ){
      data[,i]=as.factor(data[,i])
    }
  }
  # linskpotter complete
  lsc=linkspotterComplete(data, corMethods=corMethods, defaultMinCor=defaultMinCor, defaultCorMethod=defaultCorMethod, clusteringCorMethod=clusteringCorMethod, nbCluster=nbCluster, appTitle=appTitle, printInfo=F)
  return(lsc)
}
