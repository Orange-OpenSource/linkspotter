# --------------------------------------------------------------------------------
# title: Linkspotter/emptyVar
# description: detecting empty variables in a dataframe
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
# empty vars
emptyVar<-function(dataset){
  names(dataset)[unlist(lapply(dataset,function(x){sum(!is.na(x))==0}))]
}
