# --------------------------------------------------------------------------------
# title: Linkspotter/uselessVar
# description: detect useless variables (without enough entropy or variance / static variables) in a dataframe
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
uselessVar<-function(dataset){
  names(dataset)[lapply(dataset,function(x){length(levels(as.factor(x)))})==1]
}
