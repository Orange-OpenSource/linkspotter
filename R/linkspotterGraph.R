# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterGraph
# description: compute the linkspotter graph
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @import visNetwork
linkspotterGraph<-function(multiBivariateCorrelationDataFrame, variablesClustering, minCor=0.3, corMethod=colnames(multiBivariateCorrelationDataFrame)[-c(1:3,ncol(multiBivariateCorrelationDataFrame))][length(colnames(multiBivariateCorrelationDataFrame)[-c(1:3,ncol(multiBivariateCorrelationDataFrame))])], smoothEdges=T, dynamicNodes=T, colorEdgesByCorDirection=T){
  # format edges
  edges_raw=multiBivariateCorrelationDataFrame
  colnames(edges_raw)[2:3]<-c("from","to")
  if(colorEdgesByCorDirection) edges_raw=data.frame(edges_raw,color=factor(edges_raw$correlationType,levels = c("negative","positive","nominal"),labels =  c("red","blue","grey")))

  # format nodes
  if(!is.null(variablesClustering)){
    nodes_raw=data.frame(variablesClustering,label=variablesClustering[,1])
    colnames(nodes_raw)<-c("id","group","label")
  }else{
    nodes_raw=data.frame(id=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))),label=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))))
  }

  # take options into account
  ## initialize
  edges=edges_raw
  nodes=nodes_raw

  # complete abbreviations
  corMethod=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")[pmatch(tolower(corMethod),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")))]

  ## apply floating parameters
  edges=data.frame(edges,value=abs(edges[,corMethod]))
  edges=edges[!is.na(edges$value)&edges$value>=minCor,]

  #plot
  visNetwork::visNetwork(nodes,edges, selectConnectedEdges=F, width = '100%', height = '100%') %>%
    visNetwork::visEdges(smooth = smoothEdges) %>%
    visNetwork::visEvents(stabilizationIterationsDone=paste0("function () {this.setOptions( { physics: ",tolower(dynamicNodes)," } );}"))

}
#######
