# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterGraph
# description: compute the linkspotter graph
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title Linspotter graph runner
#' @description  Run the linkSpotter graph
#'
#' @param corDF a specific dataframe containing correlations values resulting from the function multiBivariateCorrelation()
#' @param variablesClustering a specific dataframe containing the output of the variable clustering resulting from the function clusterVariables()
#' @param minCor a double between 0 and 1. It is the minimal correlation absolute value to consider for the first graph plot.
#' @param corMethod a string. One of "pearson","spearman","kendall","mic", "distCor" or "MaxNMI". It is the correlation coefficient to consider for the first graph plot.
#' @param smoothEdges a boolean. TRUE to let the edges be smooth.
#' @param dynamicNodes a boolean. TRUE to let the graph re-organize itself after any movement.
#' @param colorEdgesByCorDirection a boolean. TRUE to get the edges colored according to the correlation direction (positive-> blue, negative->red or NA->grey).
#' @return a visNetwork object corresponding to a dynamic graph for the correlation matrix visualization.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' corDF=multiBivariateCorrelation(mixedData = iris, corMethods = "MaxNMI")
#' corMatrix=corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' corGroups=clusterVariables(correlation_matrix = corMatrix, nbCluster = 3)
#'
#' # launch the graph
#' linkspotterGraph(corDF=corDF, variablesClustering=corGroups, minCor=0.3)
#'
#' }
#'
#' @import visNetwork
#'
#' @export
#'
linkspotterGraph<-function(corDF, variablesClustering=NULL, minCor=0.3, corMethod=colnames(corDF)[-c(1:3,ncol(corDF))][length(colnames(corDF)[-c(1:3,ncol(corDF))])], smoothEdges=T, dynamicNodes=F, colorEdgesByCorDirection=F){
  # format edges
  edges_raw=corDF
  colnames(edges_raw)[2:3]<-c("from","to")
  if(colorEdgesByCorDirection) edges_raw=data.frame(edges_raw,color=factor(edges_raw$correlationType,levels = c("negative","positive","nominal"),labels =  c("red","blue","grey")))

  # format nodes
  if(!is.null(variablesClustering)){
    nodes_raw=data.frame(variablesClustering,label=variablesClustering[,1],title=variablesClustering[,1])
    colnames(nodes_raw)<-c("id","group","label","title")
  }else{
    nodes_raw=data.frame(id=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))),label=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))), title=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))))
  }

  # take options into account
  ## initialize
  edges=edges_raw
  nodes=nodes_raw

  # complete abbreviations
  corMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(corMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]

  ## apply floating parameters
  edges=data.frame(edges,value=abs(edges[,corMethod]), title=unlist(lapply(edges[,corMethod], function(x){paste(c(corMethod, ': ', format(round(x, digits = 2), nsmall = 2)),collapse="")})))
  edges=edges[!is.na(edges$value)&edges$value>=minCor,]

  #plot
  visNetwork::visNetwork(nodes,edges, selectConnectedEdges=F, width = '100%', height = '100%') %>%
    visNetwork::visEdges(smooth = smoothEdges) %>%
    visNetwork::visEvents(stabilizationIterationsDone=paste0("function () {this.setOptions( { physics: ",tolower(dynamicNodes)," } );}"))

}
#######
