# empty vars
emptyVar<-function(dataset){
  names(dataset)[unlist(lapply(dataset,function(x){sum(!is.na(x))==0}))]
}
