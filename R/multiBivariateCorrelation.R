# --------------------------------------------------------------------------------
# title: Linkspotter/multiBivariateCorrelation
# description: function to compute several type of correlation coefficients on all couples of a dataframe
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#' @title Calculation of all the bivariate correlations in a dataframe
#' @description  Computation of a correlation dataframe.
#'
#' @param dataset the dataframe which variables bivariate correlations are to be analyzed.
#' @param corMethods a vector of correlation coefficients to compute. The available coefficients are the following : \code{c("pearson","spearman","kendall","mic","distCor","MaxNMI")}. It is not case sensitive and still work if only the beginning of the word is put (e.g. \code{pears}).
#' @param showProgress a boolean to decide whether to show the progress bar.
#' @return a specific dataframe containing correlations values or each specified correlation coefficient.
#'
#' @examples
#' # run linkspotter on iris example data
#' data(iris)
#' corDF<-multiBivariateCorrelation(iris)
#' print(corDF)
#'
#' @importFrom minerva mine
#' @importFrom energy dcor
#' @importFrom stats cor
#' @importFrom utils combn
#' @importFrom pbapply pbapply
#'
#' @export
#'
multiBivariateCorrelation<-function(dataset, corMethods=c("pearson","spearman","kendall","mic","MaxNMI"), showProgress=T){

  #progress bar
  if(!showProgress){
    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo), add = TRUE)
  }else{
    pbo <- pboptions(type = "timer")
    on.exit(pboptions(pbo), add = TRUE)
  }

  # small formats
  dataset=droplevels.data.frame(data.frame(dataset))
  ## complete abbreviations
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[pmatch(tolower(corMethods),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")))]
  ## reorder
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")[c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNMI")%in%corMethods]

  # all combinaisons
  dfcmb=data.frame(t(utils::combn(colnames(dataset),2)))

  # detect type of couples
  typeOfCouple=apply(dfcmb,1,function(x){
    2*is.numeric(dataset[,x[1]])+is.numeric(dataset[,x[2]])# 3->(num,num), 2->(num,fact), 1->(fact,num), 0->(fact,fact)
  })
  typeOfCouple=factor(as.factor(typeOfCouple),levels = c("0","1","2","3"),labels =c("fact.fact","fact.num","num.fact","num.num"))
  dfcmb=data.frame(dfcmb,typeOfCouple)

  # uninteresting variables
  uninterestingVariables=c(uselessVar(dataset),emptyVar(dataset))

  # compute correlations according to type of couples
  corrs=t(pbapply::pbapply(dfcmb,1,function(x){
    if(as.character(x[1])%in%uninterestingVariables|as.character(x[2])%in%uninterestingVariables){ #so put NA for all corMethods
      cors=c()
      if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
      if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
      if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
      if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
      if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
      if("MaxNMI"%in%corMethods){MaxNMI=NA; cors=c(cors,MaxNMI=MaxNMI)}
      as.vector(cors)
    }else{
      switch(x[3],
             "num.num"={
               cors=c()
               if("pearson"%in%corMethods){pearson=stats::cor(x=dataset[,as.character(x[1])],y=dataset[,as.character(x[2])],use = "pairwise.complete.obs",method = "pearson"); cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=stats::cor(x=dataset[,as.character(x[1])],y=dataset[,as.character(x[2])],use = "pairwise.complete.obs",method = "spearman"); cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=stats::cor(x=dataset[,as.character(x[1])],y=dataset[,as.character(x[2])],use = "pairwise.complete.obs",method = "kendall"); cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=energy::dcor(x=dataset[,as.character(x[1])],y=dataset[,as.character(x[2])]); cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=minerva::mine(x=dataset[,as.character(x[1])],y=dataset[,as.character(x[2])],use = "pairwise.complete.obs")$MIC; cors=c(cors,mic=mic)}
               if("MaxNMI"%in%corMethods){MaxNMI=maxNMI(dataset[,as.character(x[1])],dataset[,as.character(x[2])]); cors=c(cors,MaxNMI=MaxNMI)}
               as.vector(cors)
             },
             "num.fact"={
               cors=c()
               if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
               if("MaxNMI"%in%corMethods){MaxNMI=maxNMI(dataset[,as.character(x[1])],dataset[,as.character(x[2])]); cors=c(cors,MaxNMI=MaxNMI)}
               as.vector(cors)
             },
             "fact.num"={
               cors=c()
               if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
               if("MaxNMI"%in%corMethods){MaxNMI=maxNMI(dataset[,as.character(x[2])],dataset[,as.character(x[1])]); cors=c(cors,MaxNMI=MaxNMI)}
               as.vector(cors)
             },
             "fact.fact"={
               cors=c()
               if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
               if("MaxNMI"%in%corMethods){MaxNMI=maxNMI(dataset[,as.character(x[1])],dataset[,as.character(x[2])]); cors=c(cors,MaxNMI=MaxNMI)}
               as.vector(cors)
             }
      )
    }
  }))

  if(length(corMethods)==1) corrs=t(corrs)
  colnames(corrs)<-corMethods
  dfcmb=data.frame(dfcmb,corrs)

  # detect negative (monotomic) relationships
  corToUse=c("spearman","pearson","kendall") #in this order
  corToUse=corToUse[corToUse%in%colnames(dfcmb)]
  if(length(corToUse)==0){
    correlationType=rep("unknown",nrow(dfcmb))
  }else{
    correlationType=sign(dfcmb[,corToUse[1]])
    correlationType[correlationType==-1]<-"negative"
    correlationType[correlationType==1]<-"positive"
    correlationType[is.na(correlationType)]<-"nominal"
  }
  dfcmb=data.frame(dfcmb,correlationType)

  # return result formatted
  dfcmb=data.frame(id=as.character(1:nrow(dfcmb)),dfcmb)
  return(dfcmb)
}
