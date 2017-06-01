# --------------------------------------------------------------------------------
# title: Linkspotter/createShinyAppFolder
# description: function to load and properly proccess a file
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
#
#' @title Ready-for-deployment shiny app folder creation
#' @description  This function creates a shiny app folder containing a shiny app object directly readable by a shiny-server.
#'
#' @param linkspotterShinyAppObject a shiny.appobj object, resulting from linkspotterUI(), linkspotterComplete()$run_it or linkspotterOnFile()$run_it functions.
#' @param folderName a character string corresponding to the name of the shiny app folder to create.
#'
#' @export
createShinyAppFolder<-function(linkspotterShinyAppObject, folderName){
  dir.create(folderName, recursive = T)
  dir.create(file.path(folderName, 'src'), showWarnings = FALSE)
  outputFileName=file.path(file.path(folderName, 'src'),"linkspotterShinyAppObject.rdata")
  save(linkspotterShinyAppObject,file = outputFileName)
  write("####################################################",file = file.path(folderName,"app.r"))
  write("######### ShinyApp from Linkspotter Output #########",file = file.path(folderName,"app.r"),append = T)
  write("### Copyright 2017 Alassane Samba - Orange Labs ####",file = file.path(folderName,"app.r"),append = T)
  write("####################################################",file = file.path(folderName,"app.r"),append = T)
  write("load('./src/linkspotterShinyAppObject.rdata')",file = file.path(folderName,"app.r"),append = T)
  write("library(linkspotter)",file = file.path(folderName,"app.r"),append = T)
  write("linkspotterShinyAppObject",file = file.path(folderName,"app.r"),append = T)
}
