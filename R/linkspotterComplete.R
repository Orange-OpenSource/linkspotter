# Software Name: Linkspotter
# SPDX-FileCopyrightText: Copyright (c) 2017 Orange
# SPDX-License-Identifier: MIT License
#
# This software is distributed under the MIT license.
#
# Author: Alassane SAMBA <alassane.samba(at)orange(dot)com>
#
#' @title Linkspotter complete runner
#' @description  Computation of correlation matrices, variable clustering and the customizable user
#' interface to visualize them using a graph together with variables distributions and cross plots.
#'
#' @param dataset the dataframe which variables bivariate correlations are to be analyzed.
#' @param targetVar a vector of character strings corresponding to the names of the target variables.
#' If not NULL, correlation coefficients are computed only with that target variables.
#' @param corMethods a vector of correlation coefficients to compute. The available coefficients are
#' the following : \code{c("pearson","spearman","kendall","mic","distCor","MaxNMI")}. It is not case
#' sensitive and still work if only the beginning of the word is put (e.g. \code{pears}).
#' @param maxNbBins an integer used if corMethods include 'MaxNMI'. It corresponds to the number of
#' bins limitation (for computation time limitation), maxNbBins=100 by default.
#' @param defaultMinCor a double between 0 and 1. It is the minimal correlation absolute value to
#' consider for the first graph plot.
#' @param defaultCorMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or
#' "MaxNMI". It is the correlation coefficient to consider for the first graph plot.
#' @param clusteringCorMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or
#' "MaxNMI". It is the correlation coefficient to consider for the variables clustering.
#' @param nbCluster an integer. It is the number of clusters to compute.
#' @param printInfo a boolean indicating whether to print on the console some information about
#' the dataset and the estimated computation time.
#' @param appTitle a string taken as the title of the user interface.
#' @param htmlTop a character string that enable to customize your shiny app by adding an
#' HTML code in the HEAD tag.
#' @param htmlBottom a character string that enable to customize your shiny app by adding an
#' HTML code at the end of the BODY tag.
#' @return a list containing all the material enabling to analyze correlations:
#' \itemize{
#'   \item{\code{computationTime}: a string}
#'   \item{\code{run_it}: a shiny.appobj object enable to deploy instantly the user interface for a
#'   customizable visualization.}
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
#' # run linkspotter on iris example data
#' data(iris)
#' lsOutputIris<-linkspotterComplete(iris)
#' summary(lsOutputIris)
#' \dontrun{
#' # launch the UI
#' lsOutputIris$launchShiny(option=list(port=8000))
#' }
#'
#' @export
linkspotterComplete<-function(dataset, targetVar=NULL,
                              corMethods=c("pearson","spearman","kendall","mic","MaxNMI"),
                              maxNbBins=100, defaultMinCor=0.3, defaultCorMethod=corMethods[length(corMethods)],
                              clusteringCorMethod=defaultCorMethod, nbCluster=1:9, printInfo=T, appTitle="Linkspotter",
                              htmlTop="", htmlBottom=""){
  startTime<-Sys.time()
  #complete abbreviations
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(corMethods),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  defaultCorMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(defaultCorMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  clusteringCorMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(clusteringCorMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  #compute corDF
  corDF=multiBivariateCorrelation(dataset = dataset, targetVar=targetVar, corMethods = corMethods, maxNbBins = maxNbBins, showProgress=printInfo)
  if(is.null(targetVar)){
    #corr matrix for clustering
    corMatrix=corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',clusteringCorMethod)])# prefer MaxNMI or distCor for the clustering because they hilights different types of correlation (not only linear and monotonic ones) and because prefer MaxNMI because it is always available/computable (whatever the type of variable)
    #perform clustering
    corGroups=clusterVariables(corMatrix = corMatrix, nbCluster = nbCluster)
    if(printInfo) cat(paste("Clustering computation finished:",Sys.time()));cat("\n")
  }else{
    corGroups=NULL
    clusteringCorMethod=NULL
  }
  endTime<-Sys.time()
  #compute corr matrices
  corMatrices=lapply(corMethods,function(x){corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',x)])})
  names(corMatrices)<-corMethods
  computationTime=format(round(endTime-startTime,3),nsmall = 3)
  #compute UI
  launchShiny=function(...){linkspotterUI(dataset,corDF,corGroups, defaultMinCor = defaultMinCor, appTitle=appTitle, htmlTop=htmlTop, htmlBottom=htmlBottom, ...)}
  if(printInfo) cat(paste(c("Total Computation time: ", computationTime),collapse=""));cat("\n")
  #finish
  return(list(computationTime=computationTime, launchShiny=launchShiny, dataset=dataset, targetVar=targetVar, corDF=corDF, corMatrices=corMatrices, corGroups=corGroups, clusteringCorMethod=clusteringCorMethod, defaultMinCor=defaultMinCor, defaultCorMethod=defaultCorMethod, corMethods=corMethods))
}
###
