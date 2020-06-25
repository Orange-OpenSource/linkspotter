# --------------------------------------------------------------------------------
# title: Linkspotter/BeEFdiscretization.numnum
# description: Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#' @title BeEF: Best Equal-Frequency discretization (for a couple of quantitative variables)
#' @description  Discretize two quantitative variables by optimizing the obtained the Normalized Mutual Information
#'
#' @param continuousX a vector of numeric.
#' @param continuousY a vector of numeric.
#' @param includeNA a boolean. TRUE to include NA value as a factor level.
#' @param maxNbBins an integer corresponding to the number of bins limitation (for computation time limitation), maxNbBins=100 by default.
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a list of two factors.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' disc=BeEFdiscretization.numnum(iris$Sepal.Length,iris$Sepal.Width)
#' summary(disc$x)
#' summary(disc$y)
#'
#' @importFrom Hmisc cut2
#' @importFrom stats complete.cases
#' @importFrom pbapply pblapply pboptions
#'
#' @export
#'
BeEFdiscretization.numnum<-function(continuousX,continuousY,maxNbBins=100, includeNA=T, showProgress=F){

  #progress bar
  if(!showProgress){
    pbo <- pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }else{
    pbo <- pbapply::pboptions(type = "timer")
    on.exit(pbapply::pboptions(pbo), add = TRUE)
  }

  #identifier complete obs
  cc=complete.cases(data.frame(continuousX,continuousY))
  N=sum(cc)

  #if no variability
  if((length(levels(droplevels(as.factor(continuousX))))<2)|(length(levels(droplevels(as.factor(continuousY))))<2))
    return(list(nx=NA,ny=NA,MaxNMI=NA))

  #util: number of digits (to avoid bug of cut2)
  nbdigitsX<-max(nchar(sub('^0+','',sub('\\.','',continuousX[cc]))))
  nbdigitsY<-max(nchar(sub('^0+','',sub('\\.','',continuousY[cc]))))

  #threshold
  threshold=min(c((N^0.6),maxNbBins,length(unique(continuousX))*length(unique(continuousY))),na.rm=T)
  if(threshold<4) threshold<-4 #(2*2)
  # Algo
  eg=expand.grid(2:ceiling(threshold/2),2:ceiling(threshold/2))
  egt=eg[,1]*eg[,2]
  eg2=eg[egt<=threshold,]
  NMIs = pbapply::pblapply(as.data.frame(t(eg2)), function(n) {
    xfact<-EFdiscretization(continuousX,n[1],nbdigitsX)
    yfact<-EFdiscretization(continuousY,n[2],nbdigitsY)
    nmi = NormalizedMI(xfact, yfact, includeNA = includeNA)
    list(nx = n[1], ny = n[2], NMI = nmi)
  })
  NMIsDF = as.data.frame(matrix(unlist(NMIs), ncol = 3, byrow = T))
  colnames(NMIsDF) <- c("nx", "ny", "MaxNMI")
  best = NMIsDF[which.max(NMIsDF$MaxNMI), ]
  b_xfact<-EFdiscretization(continuousX,best$nx,nbdigitsX)
  b_yfact<-EFdiscretization(continuousY,best$ny,nbdigitsY)
  return(list(x = b_xfact, y = b_yfact))
}
