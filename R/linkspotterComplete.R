# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterComplete
# description: global function combining all others : compute data and generate UI
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
# global function combining all others : compute data and generate UI
linkspotterComplete<-function(dataset, corMethods=c("pearson","spearman","kendall","mic","MaxNormMutInfo"), defaultMinCor=0.3, defaultCorMethod="MaxNormMutInfo", clusteringCorMethod=NULL, nbCluster=1:9, printInfo=T){# clusteringCorMethod default with preference order
  startTime<-Sys.time()
  p=ncol(dataset)
  nbCouples=((p*p)-p)/2
  nbObs=nrow(dataset)
  unitDuration=0.050 # in seconds
  durationEstim=nbObs/100*nbCouples*unitDuration
  if(printInfo){
    print(paste(c("Number of variables: ", p), collapse=""))
    print(paste(c("Number of couples: ", nbCouples), collapse=""))
    print(paste(c("Number of observations: ", nbObs), collapse=""))
    print(paste(c("Duration estimation: ",format(as.POSIXct(durationEstim, origin = "1970-01-01", tz = "UTC"), "%T")), collapse=""))
    print(paste("Start time:",startTime))
  }
  #complete abbreviations
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")[pmatch(tolower(corMethods),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")))]
  corDF=multiBivariateCorrelation(mixedData = dataset, corMethods = corMethods)
  if(is.null(clusteringCorMethod)){
    clusteringCorMethod=c("MaxNormMutInfo","distCor","spearman","kendall","pearson","mic")[c("MaxNormMutInfo","distCor","spearman","kendall","pearson","mic")%in%corMethods][1]
  }else{
    #complete abbreviations
    clusteringCorMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")[pmatch(tolower(clusteringCorMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")))]
  }
  if(printInfo) print(paste("Correlations computation finished:",Sys.time()))
  corMatrix=matrixOfValuesOfAllCouples(x1_x2_val = corDF[,c('X1','X2',clusteringCorMethod)])# prefer MaxNormMutInfo or distCor for the clustering because they hilights different types of correlation (not only linear and monotonic ones) and because prefer MaxNormMutInfo because it is always available/computable (whatever the type of variable)
  corGroups=clusterVariables(correlation_matrix = corMatrix, nbCluster = nbCluster)
  if(printInfo) print(paste("Clustering computation finished:",Sys.time()))
  endTime<-Sys.time()
  corMatrixes=lapply(corMethods,function(x){matrixOfValuesOfAllCouples(x1_x2_val = corDF[,c('X1','X2',x)])})
  names(corMatrixes)<-corMethods
  computationTime=format(round(endTime-startTime,3),nsmall = 3)
  run_it=linkspotterUI(dataset,corDF,corGroups,clusteringCorMethod = clusteringCorMethod, defaultMinCor = defaultMinCor,defaultCorMethod = defaultCorMethod)
  if(printInfo) print(paste(c("Total Computation time: ", computationTime),collapse=""))
  return(list(computationTime=computationTime,run_it=run_it,dataset=dataset,corDF=corDF,corMatrixes=corMatrixes,corGroups=corGroups,clusteringCorMethod=clusteringCorMethod,defaultMinCor=defaultMinCor,defaultCorMethod=defaultCorMethod,corMethods=corMethods))
}
###
