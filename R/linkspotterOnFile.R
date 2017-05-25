# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterOnFile
# description: function to load and properly proccess a file
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#
#' @title Proccess Linkspotter on an external file
#' @description  This function imports an external dataset, computates its correlation matrices, variable clustering and the customizable user inferface to visualize them using a graph.
#'
#' @param file the file containing a structured dataset which the bivariate correlations are to be analyzed.
#' @param header a boolean.TRUE if the file contains a header (similar to read.table)
#' @param sep the separation character used in the file (similar to read.table)
#' @param quote the quote character used in the file (similar to read.table)
#' @param corMethods a vector of correlation coefficients to compute. The available coefficients are the following : \code{c("pearson","spearman","kendall","mic","distCor","MaxNMI")}. It is not case sensitive and still work if only the beginning of the word is put (e.g. \code{pears}).
#' @param defaultMinCor a double between 0 and 1. It is the minimal correlation absolute value to consider for the first graph plot.
#' @param defaultCorMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or "MaxNMI". It is the correlation coefficient to consider for the first graph plot.
#' @param clusteringCorMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or "MaxNMI". It is the correlation coefficient to consider for the variables clustering.
#' @param nbCluster an integer. It is the number of clusters to compute.
#' @param printInfo a boolean indicating whether to print on the console some information about the dataset and the estimated computation time.
#' @param appTitle a string taken as the title of the user interface.
#' @param htmlTop a character string that enable to customize your shiny app by adding an HTML code in the HEAD tag.
#' @param htmlBottom a character string that enable to customize your shiny app by adding an HTML code at the end of the BODY tag.
#' @param ...	Further arguments to be passed to the used read.table function.
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
#' lsOutputIrisFromFile<-linkspotterOnFile("iris.csv")
#'
#' # launch the UI
#' lsOutputIrisFromFile$run_it
#'
#' }
#'
#' @export
#'
#' @import utils
linkspotterOnFile<-function(file, header, sep, quote, corMethods=c("pearson","spearman","kendall","mic","MaxNMI"), defaultMinCor=0.3, defaultCorMethod=corMethods[length(corMethods)], clusteringCorMethod=corMethods[length(corMethods)], nbCluster=1:9, printInfo=T, appTitle="Linkspotter", htmlTop="", htmlBottom="", ...){
  # commodity
  if (is.null(file))
    return(NULL)
  # read file
  data=read.table(stringsAsFactors = T, file = file$datapath, header=header, sep=sep,
                  quote=quote, ...)
  # first format
  for(i in 1:ncol(data)){
    if(is.factor(data[,i]) | (sum(is.na(data[,i]))==length(data[,i])) | is.logical(data[,i]) | length(levels(as.factor(data[,i])))<20 ){
      data[,i]=as.factor(data[,i])
    }
  }
  # linskpotter complete
  lsc=linkspotterComplete(data, corMethods=corMethods, defaultMinCor=defaultMinCor, defaultCorMethod=defaultCorMethod, clusteringCorMethod=clusteringCorMethod, nbCluster=nbCluster, appTitle=appTitle, printInfo=printInfo, htmlTop=htmlTop, htmlBottom=htmlBottom)
  return(lsc)
}
