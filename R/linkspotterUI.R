# --------------------------------------------------------------------------------
# title: Linkspotter/linkspotterUI
# description: run the linkSpotter user interface
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Alassane Samba, Orange
# ---------------------------------------------------------------------------------
#' @title Linspotter user interface runner
#' @description  Run the linkSpotter user interface
#'
#' @param dataset the dataframe which variables bivariate correlations are contained in corDF
#' @param corDF a specific dataframe containing correlations values resulting from the function multiBivariateCorrelation()
#' @param variablesClustering a specific dataframe containing the output of the variable clustering resulting from the function clusterVariables()
#' @param defaultMinCor a double between 0 and 1. It is the minimal correlation absolute value to consider for the first graph plot.
#' @param appTitle a character string taken as the title of the user interface.
#' @param htmlTop a character string that enable to customize your shiny app by adding an HTML code in the HEAD tag.
#' @param htmlBottom a character string that enable to customize your shiny app by adding an HTML code at the end of the BODY tag.
#' @param ... : arguments for 'shiny::shinyApp' function
#' @return a 'shiny.appobj' object enable to deploy instantly the user interface for a customizable visualization.
#'
#' @examples
#' # calculate a correlation dataframe
#' data(iris)
#' corDF=multiBivariateCorrelation(dataset = iris)
#' corMatrix=corCouplesToMatrix(x1_x2_val = corDF[,c('X1','X2',"MaxNMI")])
#' corGroups=clusterVariables(corMatrix = corMatrix, nbCluster = 3)
#' \dontrun{
#' # launch the UI
#' linkspotterUI(dataset=iris, corDF=corDF, variablesClustering=corGroups,
#' defaultMinCor=0.3,cappTitle="Linkspotter on iris data",
#' options = list(port=8000)
#' )
#' }
#'
#' @import shiny
#' @import visNetwork
#' @import rAmCharts
#' @import ggplot2
#' @importFrom utils capture.output head
#' @importFrom stats na.omit
#' @importFrom shinybusy add_busy_spinner
#'
#' @export
#'
linkspotterUI<-function(dataset, corDF, variablesClustering=NULL, defaultMinCor=0.3, appTitle="Linkspotter", htmlTop="", htmlBottom="", ...){

  # small formats and checks
  dataset=data.frame(droplevels.data.frame(dataset))

  # useful variables
  availableCorMethods=colnames(corDF)[-c(1:3,ncol(corDF))]
  defaultCorMethod=availableCorMethods[length(availableCorMethods)] # correlation coefficient to use in the graph

  # shiny server
  server <- function(input, output) {

    # format edges
    edges_raw=corDF
    colnames(edges_raw)[2:3]<-c("from","to")
    edges_raw=data.frame(edges_raw,color=factor(edges_raw$correlationType,levels = c("negative","positive","nominal"),labels =  c("red","blue","grey")))

    # format nodes
    if(!is.null(variablesClustering)){
      nodes_raw=data.frame(variablesClustering,label=variablesClustering[,1],title=variablesClustering[,1])
      colnames(nodes_raw)<-c("id","group","label","title")
    }else{
      nodes_raw=data.frame(id=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))),label=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))),title=unique(c(as.character(edges_raw$from),as.character(edges_raw$to))))
    }

    # create network plot
    output$network <- renderVisNetwork({

      # initialize
      edges=edges_raw
      nodes=nodes_raw

      # apply initial parameters
      edges=data.frame(edges,value=abs(edges[,defaultCorMethod]),title=unlist(lapply(edges[,defaultCorMethod], function(x){paste(c(defaultCorMethod, ': ', format(round(x, digits = 2), nsmall = 2)),collapse="")})))
      edges=edges[!is.na(edges$value)&edges$value>=defaultMinCor,]

      # isolated nodes
      if(input$hideIsolatedNodes){
        nodes<-nodes[nodes$id%in%unique(c(as.character(edges$from),as.character(edges$to))),]
      }

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
      edges=data.frame(edges_raw,value=abs(edges_raw[,input$selectCorMethod]),title=unlist(lapply(edges_raw[,input$selectCorMethod], function(x){paste(c(input$selectCorMethod, ': ', format(round(x, digits = 2), nsmall = 2)),collapse="")})))
      edges=edges[edges$value>=input$minCor,]

      # smoothEdges
      smoothEdgesChoice=input$smoothEdges


      # interestVarMinCor
      if(!input$selectInterestVar%in%"(NONE)"){
        edges2=data.frame(edges_raw,value=abs(edges_raw[,input$selectCorMethod]),title=unlist(lapply(edges_raw[,input$selectCorMethod], function(x){paste(c(input$selectCorMethod, ': ', format(round(x, digits = 2), nsmall = 2)),collapse="")})))
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

      # isoloated Nodes
      nodes<-nodes_raw
      if(input$hideIsolatedNodes){
        nodes_not_isolated<-unique(c(as.character(edges$from),as.character(edges$to)))
        nodes_id_to_rm<-nodes$id[!nodes$id%in%nodes_not_isolated]
        nodes<-nodes[nodes$id%in%nodes_not_isolated,]
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes = nodes) %>%
          visRemoveNodes(id = nodes_id_to_rm)
      }else{
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes = nodes)
      }

      #show number of edges
      output$shiny_text_currentNbLinks<-renderText({
        nodes_not_isolated<-unique(c(as.character(edges$from),as.character(edges$to)))
        nodes_isolated<-nodes_raw$id[!nodes_raw$id%in%nodes_not_isolated]
        paste0("nb. current edges: ",length(stats::na.omit(edges$value)), "\n",
               "nb. current linked nodes: ",length(nodes_not_isolated), "\n",
               "nb. current ", paste(c("","hidden ")[input$hideIsolatedNodes], collapse = ""), "isolated nodes: ",length(nodes_isolated)
        )
      })

      # plot network
      visNetworkProxy("network") %>%
        #visRemoveEdges(id = c(1:nrow(edges_raw))) %>%
        visRemoveEdges(id = edges_raw$id[!edges_raw$id%in%edges$id]) %>%
        visUpdateEdges(edges = edges) %>%
        visSetOptions(options = list(edges = list(smooth = smoothEdgesChoice))) %>%
        visOptions(highlightNearest = highlightInterestVarChoice, nodesIdSelection = highlightInterestVarChoice)

      # showClustering
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
        resume_string <- paste(c(paste("Node:",variab),utils::capture.output(print.data.frame(resume,col.names=F,right=T,quote = F))), "\n", sep="")
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
        edge_strings <- paste(c("Edge:",utils::capture.output(print.data.frame(edge,col.names=F,right=T,quote = F))), "\n", sep="")
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
          tab=data.frame(Var1=as.vector(utils::head(names(sort(table(dataset[,variab]),decreasing = T)),n=20)),Freq=as.vector(utils::head(sort(table(dataset[,variab]),decreasing = T),n=20)))#plot at max the 20 most frequent categories
          tab=data.frame(tab, description=apply(tab,1,paste,collapse=": "))
          amBarplot(x = "Var1", y = "Freq", data = tab, xlab = variab, ylab='Frequency', labelRotation = -45, depth = 15) %>%
            amOptions(export = TRUE, main = variab)
        }
      }
    })

    # scatterplots & boxplots
    output$shiny_edges_plot<-renderPlot({
      flip=as.logical(input$flipGraph%%2)
      if((!is.null(input$edgeid))){
        edgeid=input$edgeid
        variab1=as.character(corDF$X1[corDF$id%in%c(edgeid)])
        variab2=as.character(corDF$X2[corDF$id%in%c(edgeid)])
        tab0=as.data.frame(table(x=dataset[,variab1],y=dataset[,variab2]))
        edges=corDF
        if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("num.num")){
          tab1=tab0[tab0$Freq>0,]
          tab1$x<-as.numeric(as.character(tab1$x))
          tab1$y<-as.numeric(as.character(tab1$y))
          g<-ggplot(tab1, aes_string("x", "y", size = "Freq")) +
            geom_point(alpha=0.7) +
            labs(x = variab1, y = variab2)
          if(flip) g<-g+coord_flip()
          g
        }else if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("num.fact")){
          g<-ggplot(dataset, aes_string(variab2, variab1)) +
            geom_boxplot() +
            stat_summary(fun.y=mean, geom="point", shape=23, size=4)
          if(flip) g<-g+coord_flip()
          g
        }else if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("fact.num")){
          g<-ggplot(dataset, aes_string(variab1, variab2)) +
            geom_boxplot() +
            stat_summary(fun.y=mean, geom="point", shape=23, size=4)
          if(flip) g<-g+coord_flip()
          g
        }else if((edges$typeOfCouple[edges$id%in%c(edgeid)])%in%c("fact.fact")){
          g<-ggplot(tab0, aes_string("x", "y")) +
            geom_tile(aes_string(fill = "Freq")) +
            geom_text(aes_string(label = "Freq"), color="white") +
            scale_x_discrete(expand = c(0,0)) +
            scale_y_discrete(expand = c(0,0)) +
            scale_fill_gradient("Freq", low = "lightblue", high = "blue") +
            theme_bw() +
            labs(x = variab1, y = variab2)
          if(flip) g<-g+coord_flip()
          g
        }
      }
    })

    # some insights on the data
    output$shiny_comments<-renderText({
      paste(c(
        paste("nb. observations:",nrow(data.frame(dataset))),
        paste("\nnb. variables:",ncol(data.frame(dataset))),
        paste("\nnb. couples:",((ncol(data.frame(dataset))*ncol(data.frame(dataset)))-ncol(data.frame(dataset)))/2),
        paste("\nnb. corr. calculated:",length(stats::na.omit(corDF[,input$selectCorMethod])))
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
                    choices = c("Max Normalized Mutual Information"="MaxNMI",
                                "Pearson's r [numeric variables only]"="pearson",
                                "Spearman's rho [numeric variables only]"="spearman",
                                "Kendall's tau [numeric variables only]"="kendall",
                                "Distance correlation [numeric variables only]"="distCor",
                                "Maximal Information Coefficient (MIC) [numeric variables only]"="mic"
                    )[c("MaxNMI","pearson", "spearman", "kendall", "distCor", "mic")%in%availableCorMethods],
                    multiple = FALSE,
                    selected = c(input$selectCorMethod))
      )
    })

    # Table 1 : correlation matrix
    output$shiny_corMatrix<-renderTable({
      if(!is.null(input$selectCorTableMethod)){
        cordf=corDF[,c('X1','X2',input$selectCorTableMethod)]
        cormatrix=corCouplesToMatrix(cordf)
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
    tags$head(HTML(htmlTop)),
    navbarPage(title = appTitle,
               tabPanel("Graphs",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "selectCorMethod",
                                        label = "Correlation coefficient:",
                                        choices = c("Max Normalized Mutual Information"="MaxNMI",
                                                    "Pearson's r [numeric variables only]"="pearson",
                                                    "Spearman's rho [numeric variables only]"="spearman",
                                                    "Kendall's tau [numeric variables only]"="kendall",
                                                    "Distance correlation [numeric variables only]"="distCor",
                                                    "Maximal Information Coefficient (MIC) [numeric variables only]"="mic"
                                        )[c("MaxNMI","pearson", "spearman", "kendall", "distCor", "mic")%in%availableCorMethods],
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
                            checkboxInput("hideIsolatedNodes","Hide isolated nodes",length(unique(unlist(corDF[,1:2])))>50),
                            uiOutput("corDirectionSelect_ui"),
                            actionButton(inputId = "buttonStalilize", label = "Re-stabilize"),
                            hr(),
                            HTML("<strong>General information:</strong>"),
                            verbatimTextOutput("shiny_comments"),
                            verbatimTextOutput("shiny_text_currentNbLinks"),
                            HTML("<strong>Select edge/node:</strong>"),
                            br(),br(),
                            actionButton("flipGraph","Flip the bivariate plot"),
                            br(),br(),
                            verbatimTextOutput("shiny_edge_summary"),
                            verbatimTextOutput("shiny_node_summary")
                          ),
                          mainPanel(
                            fluidRow(
                              visNetworkOutput("network"),
                              shinybusy::add_busy_spinner(spin = "fading-circle", margins = c("40%", "30%"), height = "6%", width = "6%")
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
             div(HTML("<small> Shiny app generated by <a href='http://linkspotter.sigmant.net/' target='_blank'>Linkspotter</a></small>"))

    ),
    tags$head(HTML(htmlBottom))
  )

  shinyApp(ui = ui, server = server, ... )

  }
#########
