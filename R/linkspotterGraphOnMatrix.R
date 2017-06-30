# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterGraphOnMatrix
# description: compute the linkspotter graph
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title Linspotter graph on matrix
#' @description  Run the linkSpotter graph on a correlation matrix.
#'
#' @param corMatrix a dataframe corresponding to a matrix of correlation or distance.
#' @param cluster a boolean to decide if to cluster variables.
#' @param nbCluster an integer corresponding to the number of clusters to consider.
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
#' corDF=multiBivariateCorrelation(mixedData = iris, corMethods = "pearson")
#' corMatrix=corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"pearson")])
#' # launch the graph
#' linkspotterGraph(corMatrix=corMatrix, minCor=0.3)
#'
#' }
#'
#' @export
#'
#' @import visNetwork
linkspotterGraphOnMatrix<-function(corMatrix, cluster=F, nbCluster=1:9, variablesClustering=NULL, minCor=0.3, corMethod="Coef", smoothEdges=T, dynamicNodes=T, colorEdgesByCorDirection=T){
  corMatrix<-as.data.frame(corMatrix)

  # format edges
  edges_raw=matrixToCorCouples(corMatrix,corMethod)
  colnames(edges_raw)[1:2]<-c("from","to")

  # format nodes
  if(!is.null(variablesClustering)){
    nodes_raw=data.frame(variablesClustering,label=variablesClustering[,1],title=variablesClustering[,1])
    colnames(nodes_raw)<-c("id","group","label","title")
  }else if(cluster){
    #perform clustering
    variablesClustering=clusterVariables(correlationMatrix = corMatrix, nbCluster = nbCluster)
    nodes_raw=data.frame(variablesClustering,label=variablesClustering[,1],title=variablesClustering[,1])
    colnames(nodes_raw)<-c("id","group","label","title")
    }
  else{
    nodes_raw=data.frame(id=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))),label=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))), title=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))))
  }

  # take options into account
  ## initialize
  edges=edges_raw
  nodes=nodes_raw

  ## apply floating parameters
  edges=data.frame(edges,value=abs(edges[,corMethod]), title=unlist(lapply(edges[,corMethod], function(x){paste(c(corMethod, ': ', format(round(x, digits = 2), nsmall = 2)),collapse="")})))
  edges=edges[!is.na(edges$value)&edges$value>=minCor,]

  #plot
  visNetwork::visNetwork(nodes,edges, selectConnectedEdges=F, width = '100%', height = '100%') %>%
    visNetwork::visEdges(smooth = smoothEdges) %>%
    visNetwork::visEvents(stabilizationIterationsDone=paste0("function () {this.setOptions( { physics: ",tolower(dynamicNodes)," } );}"))

}
#######
