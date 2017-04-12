# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterOnFile
# description: function to load and properly proccess a file
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
# function to load and properly proccess the file
linkspotterOnFile<-function(file1, header, sep, quote, corMethods, clusteringCorMethod, defaultCorMethod="MaxNormMutInfo"){
  # commodity
  if (is.null(file1))
    return(NULL)
  # read file
  data=read.table(stringsAsFactors = T, file = file1$datapath, header=header, sep=sep,
                  quote=quote)
  # first format
  for(i in 1:ncol(data)){
    if(is.factor(data[,i]) | (sum(is.na(data[,i]))==length(data[,i])) | is.logical(data[,i]) | length(levels(as.factor(data[,i])))<20 ){
      data[,i]=as.factor(data[,i])
    }
  }
  # linskpotter complete
  lsc=linkspotterComplete(data, corMethods=corMethods, defaultMinCor=0.5, defaultCorMethod=defaultCorMethod, clusteringCorMethod=clusteringCorMethod, nbCluster=1:9, printInfo=F)
  corMatrix=lsc$corMatrixes[names(lsc$corMatrixes)%in%defaultCorMethod]
  return(list(dataset=data,corDF=lsc$corDF, corMatrix=corMatrix, corGroups=lsc$corGroups,clusteringCorMethod=clusteringCorMethod,defaultMinCor=defaultMinCor,defaultCorMethod=defaultCorMethod,corMethods=corMethods))
}
