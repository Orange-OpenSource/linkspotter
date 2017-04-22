# --------------------------------------------------------------------------------
# title: Linkspotter/NormalizedMI
# description: calculate the normalized mutual information between 2 categorical variables
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#' @import infotheo
NormalizedMI<-function(x,y, includeNA=T){#GOOD! -> ratio d'incertitude/entropie expliquee

  #rule NA problems
  if(includeNA){
    if(includeNA&(sum(is.na(x))>0)){
      x=as.character(x)
      x[is.na(x)] <- c("NA")
      x=as.factor(x)
    }
    if(includeNA&(sum(is.na(y))>0)){
      y=as.character(y)
      y[is.na(y)] <- c("NA")
      y=as.factor(y)
    }
  }

  #if no variability
  if((length(levels(droplevels(as.factor(x))))<2)||(length(levels(droplevels(as.factor(y))))<2))
    return(NA)

  cc=complete.cases(cbind(x,y))
  x2=droplevels(as.factor(x[cc]))
  y2=droplevels(as.factor(y[cc]))
  infotheo::mutinformation(x2,y2)/log(min(c(length(levels(x2)),length(levels(y2))))) # (normalization) see section 2.2.2 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/bin/NIHMS358982-supplement-Supplemental_Figures_and_Tables.pdf
}
