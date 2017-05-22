# --------------------------------------------------------------------------------
# title: Linkspotter/BeEFdiscretization.numfact
# description: Discretize a quantitative variable by optimizing the obtained the Normalized Mutual Information with a target qualitative variable
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @title BeEF: Best Equal-Frequency discretization
#' @description  Discretize a quantitative variable by optimizing the obtained the Normalized Mutual Information with a target qualitative variable
#'
#' @param continuousY a vector of numeric.
#' @param factorX a vector of factor.
#' @param includeFactorNA a boolean. TRUE to include NA value as a factor level.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' \dontrun{
#'
#' # calculate a correlation dataframe
#' data(iris)
#' sepallength=BeEFdiscretization.numfact(continuousY=iris$Sepal.Length,factorX=iris$Species)
#' summary(sepallength)
#' }
#'
#' @export
#'
#' @importFrom Hmisc cut2
#' @import stats
BeEFdiscretization.numfact<-function(continuousY,factorX, includeFactorNA=T){

  #if includeFactorNA
  if(includeFactorNA&(sum(is.na(factorX))>0)){
    factorX=as.character(factorX)
    factorX[is.na(factorX)] <- c("NA")
    factorX=as.factor(factorX)
  }

  #if no variability
  if((length(levels(droplevels(as.factor(factorX))))<2))
    return(list(ny=NA,MaxNMI=NA))

  # Adapt for pair.wise.complete.obs computation
  cc=complete.cases(cbind(continuousY,factorX))
  continuousY=continuousY[cc]
  factorX=droplevels(as.factor(factorX[cc]))

  # Algo
  N=length(continuousY)
  nX=length(levels(droplevels(as.factor(factorX))))
  if(nX<(N^0.6)){
    nY=2:ceiling((N^0.6)/nX) # (The maximal grid size) see section 2.2.1 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/bin/NIHMS358982-supplement-Supplemental_Figures_and_Tables.pdf
  }else{
    nY=2
  }
  NMIs=lapply(nY,function(x){
    #breaksY=unique(quantile(continuousY,seq(0,1,1/x), type = 1, na.rm=T)); # EqualFreq binning
    #factorY=cut(continuousY,breaks = breaksY, include.lowest = T);
    factorY=Hmisc::cut2(continuousY,g=x)
    list(ny=x,MaxNMI=NormalizedMI(factorY,factorX))
  })
  NMIsDF=as.data.frame(matrix(unlist(NMIs),ncol = 2, byrow = T))
  colnames(NMIsDF)<-c("ny","MaxNMI")
  best=NMIsDF[which.max(NMIsDF$MaxNMI),]
  return(Hmisc::cut2(continuousY,g=best$ny))
}
