# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterComplete
# description: global function combining all others : compute data and generate UI
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title Linspotter complete runner
#' @description  Computation of correlation matrices, variable clustering and the customizable user inferface to visualize them using a graph together with variables distributions and cross plots.
#'
#' @param dataset the dataframe which variables bivariate correlations are to be analyzed.
#' @param corMethods a vector of correlation coefficients to compute. The available coefficients are the following : \code{c("pearson","spearman","kendall","mic","distCor","MaxNMI")}. It is not case sensitive and still work if only the beginning of the word is put (e.g. \code{pears}).
#' @param defaultMinCor a double between 0 and 1. It is the minimal correlation absolute value to consider for the first graph plot.
#' @param defaultCorMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or "MaxNMI". It is the correlation coefficient to consider for the first graph plot.
#' @param clusteringCorMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or "MaxNMI". It is the correlation coefficient to consider for the variables clustering.
#' @param nbCluster an integer. It is the number of clusters to compute.
#' @param printInfo a boolean indicating whether to print on the console some information about the dataset and the estimated computation time.
#' @param appTitle a string taken as the title of the user interface.
#' @return a list containing all the material enabling to analyze correlations:
#' \itemize{
#'   \item{\code{computationTime}: a string}
#'   \item{\code{run_it}: a shiny.appobj object enable to deploy instantly the user interface for a customizable visualization.}
#'   \item{\code{dataset}: the initial dataset}
#'   \item{\code{corDF}: a the correlation data.frame including values for all coefficients}
#'   \item{\code{corMatrices}: a list of correlation matrices}
#'   \item{\code{corGroups}: data.frame a data.frame list}
#'   \item{\code{clusteringCorMethod}: a character}
#'   \item{\code{defaultMinCor}: a numeric}
#'   \item{\code{defaultCorMethod}: a string}
#'   \item{\code{corMethods}: vector of strings}
#' }
#'
#' @examples
#' \dontrun{
#'
#' # run linkspotter on iris example data
#' data(iris)
#' lsOutputIris<-linkspotterComplete(iris)
#'
#' # launch the UI
#' lsOutputIris$run_it
#'
#' }
#'
#' @export
linkspotterComplete<-function(dataset, corMethods=c("pearson","spearman","kendall","mic","MaxNMI"), defaultMinCor=0.3, defaultCorMethod=corMethods[length(corMethods)], clusteringCorMethod=defaultCorMethod, nbCluster=1:9, printInfo=T, appTitle="Linkspotter"){
  startTime<-Sys.time()
  p=ncol(dataset)
  nbCouples=((p*p)-p)/2
  nbObs=nrow(dataset)
  unitDuration=0.050 # in seconds
  durationEstim=nbObs/100*nbCouples*unitDuration

  # puseless=length(uselessVar(dataset))
  # pempty=length(emptyVar(dataset))
  # pnum=sum(unlist(lapply(dataset[,colnames(dataset)%in%c(emptyVar(dataset),uselessVar(dataset))],is.numeric)))
  # pfact=p-pempty-puseless-pnum
  # nbMixCouples=pfact*pnum
  # nbFactCouples=((pfact*pfact)-pfact)/2
  # nbNumCouples=((pnum*pnum)-pnum)/2
  # nbUsefulCouples=nbMixCouples+nbFactCouples+nbNumCouples

  if(printInfo){
    print(paste(c("Number of variables: ", p), collapse=""))
    print(paste(c("Number of couples: ", nbCouples), collapse=""))
    print(paste(c("Number of observations: ", nbObs), collapse=""))
    print(paste(c("Duration estimation: ",format(as.POSIXct(durationEstim, origin = "1970-01-01", tz = "UTC"), "%T")), collapse=""))
    print(paste("Start time:",startTime))
  }
  #complete abbreviations
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(corMethods),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  defaultCorMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(defaultCorMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  clusteringCorMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(clusteringCorMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  #compute corDF
  corDF=multiBivariateCorrelation(dataset = dataset, corMethods = corMethods)
  if(printInfo) print(paste("Correlations computation finished:",Sys.time()))
  #corr matrix for clustering
  corMatrix=corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',clusteringCorMethod)])# prefer MaxNMI or distCor for the clustering because they hilights different types of correlation (not only linear and monotonic ones) and because prefer MaxNMI because it is always available/computable (whatever the type of variable)
  #perform clustering
  corGroups=clusterVariables(correlationMatrix = corMatrix, nbCluster = nbCluster)
  if(printInfo) print(paste("Clustering computation finished:",Sys.time()))
  endTime<-Sys.time()
  #compute corr matrices
  corMatrices=lapply(corMethods,function(x){corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',x)])})
  names(corMatrices)<-corMethods
  computationTime=format(round(endTime-startTime,3),nsmall = 3)
  #compute UI
  run_it=linkspotterUI(dataset,corDF,corGroups, defaultMinCor = defaultMinCor, appTitle=appTitle)
  if(printInfo) print(paste(c("Total Computation time: ", computationTime),collapse=""))
  #finish
  return(list(computationTime=computationTime,run_it=run_it,dataset=dataset,corDF=corDF,corMatrices=corMatrices,corGroups=corGroups,clusteringCorMethod=clusteringCorMethod,defaultMinCor=defaultMinCor,defaultCorMethod=defaultCorMethod,corMethods=corMethods))
}
###
