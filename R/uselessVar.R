# useless variables (without enough entropy or variance / static variables)
uselessVar<-function(dataset){
  names(dataset)[lapply(dataset,function(x){length(levels(as.factor(x)))})==1]
}
