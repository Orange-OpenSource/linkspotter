# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterUI
# description: run the linkSpotter user interface
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
require(shiny)
require(visNetwork)
linkspotterUI<-function(dataset, multiBivariateCorrelationDataFrame, variablesClustering, clusteringCorMethod=NULL, defaultMinCor=0.3, defaultCorMethod="MaxNormMutInfo", appTitle="Linkspotter"){

  # small formats and checks
  dataset=droplevels.data.frame(dataset)
  #complete abbreviations
  defaultCorMethod=c("pearson", "spearman", "kendall", "mic", "MaxNormMutInfo")[pmatch(tolower(defaultCorMethod),tolower(c("pearson", "spearman", "kendall", "mic", "MaxNormMutInfo")))] # correlation coefficient to use in the graph
  availableCorMethods=colnames(multiBivariateCorrelationDataFrame)[-c(1:3,ncol(multiBivariateCorrelationDataFrame))]

  # shiny server
  server <- function(input, output) {

    # format edges
    edges_raw=multiBivariateCorrelationDataFrame
    colnames(edges_raw)[2:3]<-c("from","to")
    edges_raw=data.frame(edges_raw,color=factor(edges_raw$correlationType,levels = c("negative","positive","nominal"),labels =  c("red","blue","grey")))

    # format nodes
    if(!is.null(variablesClustering)){
      nodes_raw=data.frame(variablesClustering,label=variablesClustering[,1])
      colnames(nodes_raw)<-c("id","group","label")
    }else{
      nodes_raw=data.frame(id=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))),label=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))))
    }

    # create network plot
    output$network <- renderVisNetwork({

      # initialize
      edges=edges_raw
      nodes=nodes_raw

      # apply initial parameters
      edges=data.frame(edges,value=abs(edges[,defaultCorMethod]))
      edges=edges[!is.na(edges$value)&edges$value>=defaultMinCor,]

      #plot
      visNetwork(nodes,edges, selectConnectedEdges=F, width = '500px', height = '500px') %>%
        visInteraction(selectConnectedEdges=F, hover=T)%>%
        visEvents(click = "function(element){
                  Shiny.onInputChange('nbedges', element.edges.length);
                  Shiny.onInputChange('edgeid', element.edges[0]);
                  Shiny.onInputChange('nodeid', element.nodes[0]);
                  ;}"
        ) %>%
        visEdges(smooth = F) %>%
        visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")
  })

    # dynamic change of parameters #corMethos, minCor or interestVarMinCor, colorEdges, nodesClustering, smooth value
    observe({

      # minCor
      edges=data.frame(edges_raw,value=abs(edges_raw[,input$selectCorMethod]))
      edges=edges[edges$value>=input$minCor,]

      # smoothEdges
      smoothEdgesChoice=input$smoothEdges


      # interestVarMinCor
      if(!input$selectInterestVar%in%"(NONE)"){
        edges2=data.frame(edges_raw,value=abs(edges_raw[,input$selectCorMethod]))
        edges2=edges2[edges2$from%in%c(input$selectInterestVar)|edges2$to%in%c(input$selectInterestVar),]
        edges2=edges2[edges2$value>=input$interestVarMinCor,]
        edges=rbind(edges,edges2)
      }

      #corDirectionToShow
      if(input$selectCorMethod%in%c("pearson","spearman","kendall")){
        if(!is.null(input$corDirectionToShow)){
          corDirectionToShowChoice=input$corDirectionToShow
          switch(corDirectionToShowChoice,
                 "both"={
                   edges=edges
                 },
                 "positive"={
                   edges=edges[edges$correlationType%in%c('positive'),]
                 },
                 "negative"={
                   edges=edges[edges$correlationType%in%c('negative'),]
                 })
        }
      }


      # enable highlightNearest and color edges
      highlightInterestVarChoice=input$highlightInterestVar
      colorEdgesByCorDirectionChoice=input$colorEdgesByCorDirection
      if(highlightInterestVarChoice){
        #edges=edges[,!colnames(edges)%in%c("color")]
        if("color"%in%colnames(edges)) colnames(edges)[colnames(edges)%in%'color']<-"colorDisabled"
        visNetworkProxy("network") %>%
          visRemoveEdges(id = edges_raw$id) %>%
          visUpdateEdges(edges = edges)
      }else if(!colorEdgesByCorDirectionChoice){
        #edges=edges[,!colnames(edges)%in%c("color")]
        if("color"%in%colnames(edges)) colnames(edges)[colnames(edges)%in%'color']<-"colorDisabled"
        visNetworkProxy("network") %>%
          visRemoveEdges(id = edges_raw$id) %>%
          visUpdateEdges(edges = edges)
      }else{
        if("colorDisabled"%in%colnames(edges)) colnames(edges)[colnames(edges)%in%'colorDisabled']<-"color"
      }

      #show number of edges
      output$shiny_text_currentNbLinks<-renderText({
        paste0("nb. current edges: ",length(na.omit(edges$value)))
      })

      # plot network
      visNetworkProxy("network") %>%
        #visRemoveEdges(id = c(1:nrow(edges_raw))) %>%
        visRemoveEdges(id = edges_raw$id[!edges_raw$id%in%edges$id]) %>%
        visUpdateEdges(edges = edges) %>%
        visSetOptions(options = list(edges = list(smooth = smoothEdgesChoice))) %>%
        visOptions(highlightNearest = highlightInterestVarChoice, nodesIdSelection = highlightInterestVarChoice)

      # showClustering
      nodes=nodes_raw
      if(!input$showClustering){
        nodes$group<-rep(1,nrow(nodes))
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes = nodes)
      }else{
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes = nodes)
      }

      # disable highlightNearest
      if(!input$highlightInterestVar){
        visNetworkProxy("network") %>%
          visUpdateEdges(edges = edges) %>%
          visUpdateNodes(nodes = nodes)
      }
    })

    # Dynamic re-stabilization
    observeEvent(input$buttonStalilize, {
      visNetworkProxy("network") %>%
        visStabilize()
    })

    # dynamic nodes
    observeEvent(input$dynamicNodes, {
      if(input$dynamicNodes){
        someOption="function () {this.setOptions( { physics: true } );}"
        visNetworkProxy("network") %>%
          visStabilize() %>%
          visEvents(stabilizationIterationsDone=someOption)
      }else{
        someOption="function () {this.setOptions( { physics: false } );}"
        visNetworkProxy("network") %>%
          visStabilize() %>%
          visEvents(stabilizationIterationsDone=someOption)
      }
    })

    # clicked variable summary
    output$shiny_node_summary<-renderText({
      if(!is.null(input$nodeid)){
        variab=as.character(input$nodeid)
        resume=data.frame(unclass(summary(dataset[,variab],10)),check.names = FALSE, stringsAsFactors = FALSE)
        #resume=data.frame(rownames(resume),resume[,1])
        colnames(resume)=""
        resume_string <- paste(c(paste("Node:",variab),capture.output(print.data.frame(resume,col.names=F,right=T,quote = F))), "\n", sep="")
        paste(resume_string,collapse='')
      }
    })

    # clicked relationship summary
    output$shiny_edge_summary<-renderText({
      if((!is.null(input$edgeid))){
        edge=edges_raw[edges_raw$id==input$edgeid,c("from","to",availableCorMethods)]
        colnames(edge)[1:2]<-c("Var1","Var2")
        edge=edge[,!is.na(edge)]
        edge[,unlist(lapply(edge,is.numeric))]<-lapply(edge[,unlist(lapply(edge,is.numeric))], function(x){format(round(x, digits = 2),nsmall = 2)})
        edge=data.frame(t(edge))
        colnames(edge)<-""
        edge_strings <- paste(c("Edge:",capture.output(print.data.frame(edge,col.names=F,right=T,quote = F))), "\n", sep="")
        paste(edge_strings,collapse='')
      }
    })

    # histogram plots
    output$shiny_nodes_plot<-renderAmCharts({
      if(!is.null(input$nodeid)){
        variab=as.character(input$nodeid)
        if(is.numeric(dataset[,variab])){
          amHist(x = as.numeric(dataset[,variab]), xlab = variab) %>%
            amOptions(export = TRUE)
        }else{
          tab=data.frame(Var1=as.vector(head(names(sort(table(dataset[,variab]),decreasing = T)),n=20)),Freq=as.vector(head(sort(table(dataset[,variab]),decreasing = T),n=20)))#plot at max the 20 most frequent categories
          tab=data.frame(tab, description=apply(tab,1,paste,collapse=": "))
          amBarplot(x = "Var1", y = "Freq", data = tab, xlab = variab, ylab='Frequency', labelRotation = -45, depth = 15) %>%
            amOptions(export = TRUE, main = variab)
        }
      }
    })

    # scatterplots & boxplots
    output$shiny_edges_plot<-renderPlot({
      if((!is.null(input$edgeid))){
        edgeid=input$edgeid
        variab1=as.character(multiBivariateCorrelationDataFrame$X1[multiBivariateCorrelationDataFrame$id%in%c(edgeid)])
        variab2=as.character(multiBivariateCorrelationDataFrame$X2[multiBivariateCorrelationDataFrame$id%in%c(edgeid)])
        dataset=dataset
        edges=multiBivariateCorrelationDataFrame
        if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("num.num")){
          plot(x = as.numeric(dataset[,variab1]), y = as.numeric(dataset[,variab2]), xlab = variab1, ylab=variab2, main=paste(paste(variab1,"vs"),variab2))
        }else if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("num.fact")){
          boostedBoxplot(y=as.numeric(dataset[,variab1]),x=as.factor(dataset[,variab2]), laby = variab1, labx=variab2, main=paste(paste(variab1,"vs"),variab2))
        }else if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("fact.num")){
          plot(as.numeric(dataset[,variab2])~as.factor(dataset[,variab1]), ylab = variab2, xlab=variab1, main=paste(paste(variab1,"vs"),variab2))
        }
      }
    })

    # some insights on the data
    output$shiny_comments<-renderText({
      paste(c(
        paste("nb. observations:",nrow(data.frame(dataset))),
        paste("\nnb. variables:",ncol(data.frame(dataset))),
        paste("\nnb. couples:",((ncol(data.frame(dataset))*ncol(data.frame(dataset)))-ncol(data.frame(dataset)))/2)
      ),collapse = "")
    })

    # interest var minimum correlation management
    output$interestMinCor_ui<-renderUI({
      if(is.null(input$interestVarMinCor)) {
        currentInterestVarMinCor=input$minCor
      }else{
        currentInterestVarMinCor=input$interestVarMinCor
      }
      if(!input$selectInterestVar%in%c("(NONE)")){
        tagList(
          sliderInput("interestVarMinCor",
                      "Minimum Correlation with interest variable:",
                      min = 0,  max = input$minCor, value = currentInterestVarMinCor)

        )
      }
    })
    # Positive or negative correlation direction exclusive show
    output$corDirectionSelect_ui<-renderUI({
      if(input$selectCorMethod%in%c("pearson","spearman","kendall")){
        tagList(
          radioButtons(inputId = "corDirectionToShow", label = "Filter by correlation direction:",
                       choices = c("Both" = "both",
                                   "Positive only" = "positive",
                                   "Negative only" = "negative"),selected = "both")
        )
      }
    })

    # cor method for tables
    output$changeTableCorMethod_ui<-renderUI({
      tagList(
        selectInput(inputId = "selectCorTableMethod",
                    label = "Correlation coefficient:",
                    choices = c("Max Normalized Mutual Information"="MaxNormMutInfo",
                                "Pearson's r [numeric variables only]"="pearson",
                                "Spearman's rho [numeric variables only]"="spearman",
                                "Kendall's tau [numeric variables only]"="kendall",
                                "Distance correlation [numeric variables only]"="distCor",
                                "Maximal Information Coefficient (MIC) [numeric variables only]"="mic"
                    )[c("MaxNormMutInfo","pearson", "spearman", "kendall", "distCor", "mic")%in%availableCorMethods],
                    multiple = FALSE,
                    selected = c(input$selectCorMethod))
      )
    })

    # Table 1 : correlation matrix
    output$shiny_corMatrix<-renderTable({
      if(!is.null(input$selectCorTableMethod)){
        cordf=multiBivariateCorrelationDataFrame[,c('X1','X2',input$selectCorTableMethod)]
        cormatrix=matrixOfValuesOfAllCouples(cordf)
        cormatrix<-data.frame(colnames(cormatrix),cormatrix)
        colnames(cormatrix)[1]<-""
        cormatrix
      }
    })
    # Table 2 : clustering table
    output$shiny_clusteringTable_ui<-renderUI({
      if(!is.null(variablesClustering)){
        output$shiny_clusteringTable<-renderTable({
          variablesClustering[,"group"]<-format(round(variablesClustering[,"group"],digits = 0),nsmall = 0)
          variablesClustering
        })
        tagList(
          h4("Variable clustering:"),
          tableOutput("shiny_clusteringTable")
        )
      }
    })

}

  # shiny ui
  ui <- fluidPage(
    #titlePanel(appTitle),
    navbarPage(title = appTitle,
               tabPanel("Graphs",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "selectCorMethod",
                                        label = "Correlation coefficient:",
                                        choices = c("Max Normalized Mutual Information"="MaxNormMutInfo",
                                                    "Pearson's r [numeric variables only]"="pearson",
                                                    "Spearman's rho [numeric variables only]"="spearman",
                                                    "Kendall's tau [numeric variables only]"="kendall",
                                                    "Distance correlation [numeric variables only]"="distCor",
                                                    "Maximal Information Coefficient (MIC) [numeric variables only]"="mic"
                                        )[c("MaxNormMutInfo","pearson", "spearman", "kendall", "distCor", "mic")%in%availableCorMethods],
                                        multiple = FALSE,
                                        selected = c(defaultCorMethod)),
                            sliderInput("minCor",
                                        "Minimum Correlation:",
                                        min = 0,  max = 1, value = defaultMinCor),
                            selectInput(inputId = "selectInterestVar",
                                        label = "Interest variable:",
                                        choices = c("(NONE)",colnames(dataset)),
                                        multiple = FALSE,
                                        selected = "(NONE)"),

                            uiOutput("interestMinCor_ui"),
                            HTML("<strong>Configuration:</strong>"),
                            checkboxInput("highlightInterestVar","Highlight variable on click",FALSE),
                            checkboxInput("showClustering","Variable Clustering",TRUE),
                            checkboxInput("colorEdgesByCorDirection","Color edges by correlation direction ",TRUE),
                            #uiOutput("colorEdges_ui"),
                            checkboxInput("smoothEdges","Smooth edges",FALSE),
                            checkboxInput("dynamicNodes","Dynamic nodes stabilization",FALSE),
                            uiOutput("corDirectionSelect_ui"),
                            actionButton(inputId = "buttonStalilize", label = "Re-stabilize"),
                            hr(),
                            HTML("<strong>General information:</strong>"),
                            verbatimTextOutput("shiny_comments"),
                            verbatimTextOutput("shiny_text_currentNbLinks"),
                            HTML("<strong>Select edge/node:</strong>"),
                            verbatimTextOutput("shiny_edge_summary"),
                            verbatimTextOutput("shiny_node_summary")
                          ),
                          mainPanel(
                            fluidRow(
                              visNetworkOutput("network")
                            ),
                            fluidRow(
                              column(6,
                                     amChartsOutput("shiny_nodes_plot")
                              ),
                              column(6,
                                     plotOutput("shiny_edges_plot")
                                     #amChartsOutput("shiny_edges_plot2")
                              )
                            )
                          )
                        )
               ),
               tabPanel("Tables",
                        uiOutput("changeTableCorMethod_ui"),
                        h4("Correlation matrix:"),
                        tableOutput("shiny_corMatrix"),
                        uiOutput("shiny_clusteringTable_ui")

               )
    ),
    fluidRow(align="center",
             tags$small(paste(c(
               "LinkSpotter",
               "Author : Alassane Samba (alassane.samba@orange.com)",
               "Copyright 2017"),
               #"uses infotheo, minerva, mclust, energy, Hmisc, shiny, visNetwork, rAmCharts. "),
               collapse=" - ")
             )
    )
  )

  shinyApp(ui = ui, server = server)

  }
#########
