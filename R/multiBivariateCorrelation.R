# --------------------------------------------------------------------------------
# title: Linkspotter/multiBivariateCorrelation
# description: function to compute several type of correlation coefficients on all couples of a dataframe
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @import minerva
#' @import energy
multiBivariateCorrelation<-function(mixedData, corMethods=c("pearson","spearman","kendall","mic","MaxNormMutInfo")){

  # small formats
  mixedData=droplevels.data.frame(data.frame(mixedData))
  ## complete abbreviations
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")[pmatch(tolower(corMethods),tolower(c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")))]
  ## reorder
  corMethods=c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")[c("pearson", "spearman", "kendall", "distCor", "mic", "MaxNormMutInfo")%in%corMethods]

  # all combinaisons
  dfcmb=data.frame(t(combn(colnames(mixedData),2)))

  # detect type of couples
  typeOfCouple=apply(dfcmb,1,function(x){
    2*is.numeric(mixedData[,x[1]])+is.numeric(mixedData[,x[2]])# 3->(num,num), 2->(num,fact), 1->(fact,num), 0->(fact,fact)
  })
  typeOfCouple=factor(as.factor(typeOfCouple),levels = c("0","1","2","3"),labels =c("fact.fact","fact.num","num.fact","num.num"))
  dfcmb=data.frame(dfcmb,typeOfCouple)

  # uninteresting variables
  uninterestingVariables=c(uselessVar(mixedData),emptyVar(mixedData))

  # compute correlations according to type of couples
  corrs=t(apply(dfcmb,1,function(x){
    if(as.character(x[1])%in%uninterestingVariables|as.character(x[2])%in%uninterestingVariables){ #so put NA for all corMethods
      cors=c()
      if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
      if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
      if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
      if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
      if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
      if("MaxNormMutInfo"%in%corMethods){MaxNormMutInfo=NA; cors=c(cors,MaxNormMutInfo=MaxNormMutInfo)}
      as.vector(cors)
    }else{
      switch(x[3],
             "num.num"={
               cors=c()
               if("pearson"%in%corMethods){pearson=cor(x=mixedData[,as.character(x[1])],y=mixedData[,as.character(x[2])],use = "pairwise.complete.obs",method = "pearson"); cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=cor(x=mixedData[,as.character(x[1])],y=mixedData[,as.character(x[2])],use = "pairwise.complete.obs",method = "spearman"); cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=cor(x=mixedData[,as.character(x[1])],y=mixedData[,as.character(x[2])],use = "pairwise.complete.obs",method = "kendall"); cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=dcor(x=mixedData[,as.character(x[1])],y=mixedData[,as.character(x[2])]); cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=mine(x=mixedData[,as.character(x[1])],y=mixedData[,as.character(x[2])],use = "pairwise.complete.obs")$MIC; cors=c(cors,mic=mic)}
               if("MaxNormMutInfo"%in%corMethods){MaxNormMutInfo=maxNMI_numnum(mixedData[,as.character(x[1])],mixedData[,as.character(x[2])])$MaxNMI; cors=c(cors,MaxNormMutInfo=MaxNormMutInfo)}
               as.vector(cors)
             },
             "num.fact"={
               cors=c()
               if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
               if("MaxNormMutInfo"%in%corMethods){MaxNormMutInfo=maxNMI_numfact(mixedData[,as.character(x[1])],mixedData[,as.character(x[2])])$MaxNMI; cors=c(cors,MaxNormMutInfo=MaxNormMutInfo)}
               as.vector(cors)
             },
             "fact.num"={
               cors=c()
               if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
               if("MaxNormMutInfo"%in%corMethods){MaxNormMutInfo=maxNMI_numfact(mixedData[,as.character(x[2])],mixedData[,as.character(x[1])])$MaxNMI; cors=c(cors,MaxNormMutInfo=MaxNormMutInfo)}
               as.vector(cors)
             },
             "fact.fact"={
               cors=c()
               if("pearson"%in%corMethods){pearson=NA; cors=c(cors,pearson=pearson)}
               if("spearman"%in%corMethods){spearman=NA; cors=c(cors,spearman=spearman)}
               if("kendall"%in%corMethods){kendall=NA; cors=c(cors,kendall=kendall)}
               if("distCor"%in%corMethods){distCor=NA; cors=c(cors,distCor=distCor)} # too long to compute
               if("mic"%in%corMethods){mic=NA; cors=c(cors,mic=mic)}
               if("MaxNormMutInfo"%in%corMethods){MaxNormMutInfo=NormalizedMI(mixedData[,as.character(x[1])],mixedData[,as.character(x[2])]); cors=c(cors,MaxNormMutInfo=MaxNormMutInfo)}
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
  #return(dfcmb[order(abs(dfcmb$MaxNormMutInfo),decreasing = T),])
  return(dfcmb)
}
