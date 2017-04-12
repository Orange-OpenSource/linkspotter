# --------------------------------------------------------------------------------
# title: Linkspotter/maxNMI_generic
# description: compute MaxNMI correlation coefficient whatever the type of couple of variable
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
maxNMI_generic<-function(x,y){
  typeOfCouple=2*is.numeric(x)+is.numeric(y)# 3->(num,num), 2->(num,fact), 1->(fact,num), 0->(fact,fact)
  typeOfCouple=factor(as.factor(typeOfCouple),levels = c("0","1","2","3"),labels =c("fact.fact","fact.num","num.fact","num.num"))
  switch(typeOfCouple,
         fact.fact={
           NormalizedMI(x,y,includeNA = F)
         },
         fact.num={
           maxNMI_numfact(y,x,includeNA = F)
         },
         num.fact={
           maxNMI_numfact(x,y,includeNA = F)
         },
         num.num={
           maxNMI_numnum(x,y,maxNbBins=100)
         }
  )
}
