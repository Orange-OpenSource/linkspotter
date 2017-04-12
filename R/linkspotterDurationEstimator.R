# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterDurationEstimator
# description: estimate the duration of the computation
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
linkspotterDurationEstimator<-function(dataset){
  p=ncol(dataset)
  nbCouples=((p*p)-p)/2
  nbObs=nrow(dataset)
  unitDuration=0.050 # in seconds
  durationEstim=nbObs/100*nbCouples*unitDuration
  print(paste(c("Linkspotter computation duration estimation: ",format(as.POSIXct(durationEstim, origin = "1970-01-01", tz = "UTC"), "%T")), collapse=""))
  return(durationEstim)
}
