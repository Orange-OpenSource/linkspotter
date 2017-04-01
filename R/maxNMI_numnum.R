####
# under estimates MIC
maxNMI_numnum<-function(continuousX,continuousY,maxNbBins=NA){

  # Only on complete obs
  cc=complete.cases(data.frame(continuousX,continuousY))
  continuousX=continuousX[cc]
  continuousY=continuousY[cc]

  N=sum(cc)

  #if no variability
  if((length(levels(droplevels(as.factor(continuousX))))<2)|(length(levels(droplevels(as.factor(continuousY))))<2))
    return(list(nx=NA,ny=NA,MaxNMI=NA))

  #threshold
  threshold=min(c((N^0.6),maxNbBins,length(unique(continuousX))*length(unique(continuousY))),na.rm=T)
  if(threshold<4) threshold<-4 #(2*2)
  # Algo
  eg=expand.grid(2:ceiling(threshold/2),2:ceiling(threshold/2))
  egt=eg[,1]*eg[,2]
  eg2=eg[egt<=threshold,]
  NMIs=lapply(as.data.frame(t(eg2)),function(n){
    #xfact=cut(continuousX,breaks = unique(quantile(continuousX,probs = seq(0,1,1/n[1]),type = 1)), include.lowest = T);
    #yfact=cut(continuousY,breaks = unique(quantile(continuousY,probs = seq(0,1,1/n[2]),type = 1)), include.lowest = T);
    xfact=cut2(continuousX,g = n[1]);
    yfact=cut2(continuousY,g = n[2]);
    nmi=NormalizedMI(xfact,yfact);
    list(nx=n[1],ny=n[2],NMI=nmi)
  })
  NMIsDF=as.data.frame(matrix(unlist(NMIs),ncol = 3, byrow = T))
  colnames(NMIsDF)<-c("nx","ny","MaxNMI")
  return(as.list(NMIsDF[which.max(NMIsDF$MaxNMI),]))
}
