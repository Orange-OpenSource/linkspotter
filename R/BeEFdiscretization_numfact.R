# --------------------------------------------------------------------------------
# title: Linkspotter/BeEFdiscretization.numfact
# description: Discretize a quantitative variable by optimizing the obtained the Normalized Mutual Information with a target qualitative variable
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#' @title BeEF: Best Equal-Frequency discretization
#' @description  Discretize a quantitative variable by optimizing the obtained the Normalized Mutual Information with a target qualitative variable
#'
#' @param continuousY a vector of numeric.
#' @param factorX a vector of factor.
#' @param includeFactorNA a boolean. TRUE to include NA value as a factor level.
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a double between 0 and 1 corresponding to the MaxNMI.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' discreteSepalLength=BeEFdiscretization.numfact(continuousY=iris$Sepal.Length,factorX=iris$Species)
#' summary(discreteSepalLength)
#'
#' @importFrom Hmisc cut2
#' @importFrom stats complete.cases
#'
#' @export
#'
BeEFdiscretization.numfact<-function(continuousY,factorX, includeFactorNA=T, showProgress=F){

  #progress bar
  if(!showProgress){
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
  }else{
    pbo <- pboptions(type = "timer")
    on.exit(pboptions(pbo), add = TRUE)
  }

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
  cc=stats::complete.cases(cbind(continuousY,factorX))
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
  nbdigitsY<-max(nchar(sub('^0+','',sub('\\.','',continuousY)))) #util: number of digits (to avoid bug of cut2)
  NMIs=lapply(nY,function(x){
    factorY<-EF.discretisation(continuousY,x,nbdigitsY)
    list(ny=x,MaxNMI=NormalizedMI(factorY, factorX))
  })
  NMIsDF=as.data.frame(matrix(unlist(NMIs), ncol = 2, byrow = T))
  colnames(NMIsDF)<-c("ny", "MaxNMI")
  best=NMIsDF[which.max(NMIsDF$MaxNMI),]
  return(EF.discretisation(continuousY,best$ny,nbdigitsY))
}
