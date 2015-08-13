library(data.table)
library(shiny)
library(d3heatmap)
library(networkD3)
library(DT)
library(rCharts)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidRow(column(12, tags$header(strong(HTML("<p align = 'center'>RNA-seq Data Plotting: Bringing <span style='color: red;'>D3</span> visualization to RNA-seq!")), style = "font-size: 50px; 
                                  background-color: #F0FFFF;"))),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV/TXT File for RNA-seq Analysis:',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      fileInput('file2', 'Choose CSV/TXT File for General Experimental Analysis:',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("Table")),
        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Kernel Density Estimation", showOutput("Density", "nvd3")), 
        tabPanel("Scatter Plot", sidebarLayout(
          sidebarPanel(
            textInput("text1", label = h5("Enter first sample name (For example, S1)"), value = "Enter text..."),
            verbatimTextOutput("value1"),
            textInput("text2", label = h5("Enter second sample name (For example, S2)"), value = "Enter text..."),
            verbatimTextOutput("value2")
          ),
          mainPanel(showOutput("ScatterPlot", "polycharts")))
        ),
        tabPanel("Boxplot", plotOutput("Boxplot")),
        tabPanel("Principal Component", sidebarLayout(
          sidebarPanel(
            selectizeInput("cvCutoff", label = 'Please select a cutoff for coefficient of variation (cv)',choices = c(0.1, 0.3, 0.5)),
            verbatimTextOutput("value3"),
            selectizeInput("clusterMethod", label = 'Please select a method for clustering (pca or mds)',choices = c('pca', 'mds')),
            verbatimTextOutput("value4")
            ),
          mainPanel(showOutput("PrincipalComponent", "nvd3")))
          ),
        tabPanel("Gene interaction network", sidebarLayout(
          sidebarPanel(
            sliderInput("Exprscut", "Expression level cutoff", 
                        min=0, max=100, step = 10, value=10),
            verbatimTextOutput("value5"),
            sliderInput("Corrcut", "Correlation cutoff", 
                        min=0, max=1, step = 0.1, value=0.7),
            verbatimTextOutput("value6")
          ),
          mainPanel(forceNetworkOutput("forceNetworkGene")))
        )
    )
  )
))

server <- function(input, output, session) {
  # generate heatmap using D3heatmap
  dataMat <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    fread(inFile$datapath, data.table=F)
  })
  
  design <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    fread(inFile$datapath, data.table=F)
  })
  
  output$Table <- renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    datatable(dataMat, options = list(pageLength = 5))
  })
  
  output$Heatmap <- renderD3heatmap({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    d3heatmap(dataMat, scale="row", colors=colorRampPalette(c("blue","white","red"))(1000))
  })
  
  output$Density <- renderChart({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    denStat <- density(dataMat[,1], from = min(dataMat), to = max(dataMat))
    denStat <- data.frame(x = denStat$x,
                          y = denStat$y)
    denStat1 <- sapply(1:ncol(dataMat), function(i){
      preset <- density(dataMat[,i], from = min(dataMat), to = max(dataMat))
      preset$y
    })
    colnames(denStat1) <- c(paste('S',1:ncol(dataMat),sep = ''))
    denStat1 <- stack(as.data.frame(denStat1))
    denStat <- cbind(rep(denStat[,1], times=ncol(dataMat)), denStat1)
    colnames(denStat) <- c('Exprs', 'Density', 'ind')
    np <- nPlot(Density ~ Exprs, group = 'ind', data = denStat, type = 'lineChart')
    np$addParams(dom = "Density")
    return(np)
  })
  
  output$value1 <- renderPrint({input$text1})
  
  output$value2 <- renderPrint({input$text2})
  
  output$ScatterPlot <- renderChart({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    rp <- rPlot(input$text1, input$text2, data = dataMat, type = 'point')
    rp$addParams(dom = "ScatterPlot")
    return(rp)
  })
  
  output$value3 <- renderPrint({input$cvCutoff})
  
  output$value4 <- renderPrint({input$clusterMethod})
  
  output$PrincipalComponent <- renderChart({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    design <- design()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    cvcutoff <- input$cvCutoff
    clusterMethod <- input$clusterMethod
    cv.gene <- apply(dataMat, 1, function(x) sd(x)/mean(x))
    dataMat <- dataMat[which(cv.gene>cvcutoff), ]
    dataMat <- scale(dataMat)
    if(clusterMethod == 'mds'){
      dd <- dist(t(dataMat))
      mds.result <- cmdscale(dd, k = 2, eig = TRUE)
      ppoints <- mds.result$points
    }
    else{
      pca.result <- prcomp(t(dataMat))
      ppoints <- pca.result$x[,1:2]
    }
    ppoints <- cbind(ppoints, design)
    rownames(ppoints) <- colnames(dataMat)
    colnames(ppoints) <- c('PC1', 'PC2', 'Design')
    np <- nPlot(PC2~PC1, data = as.data.frame(ppoints), group= 'Design', type = 'scatterChart')
    np$addParams(dom = "PrincipalComponent")
    np$xAxis(axisLabel = 'PC1')
    np$yAxis(axisLabel = 'PC2')
    return(np)
  })
  
  output$value5 <- renderPrint({input$Exprscut})
  
  output$value6 <- renderPrint({input$Corrcut})
  
  output$forceNetworkGene <- renderForceNetwork({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    mean.gene <- apply(dataMat, 1, mean)
    dataMat <- dataMat[mean.gene > input$Exprscut, ]
    MisLinks <- data.frame(source = rep(0:(nrow(dataMat)-1), each = nrow(dataMat)),
                           target = rep(0:(nrow(dataMat)-1), times = nrow(dataMat)),
                           value = c(cor(t(dataMat), method = 'spearman')))
    index <- which(abs(MisLinks$value)>input$Corrcut)
    MisLinks <- MisLinks[index, ]
    name <- unique(rep(rownames(dataMat),
                      each = nrow(dataMat))[index])
    MisNodes <- data.frame(name = name,
                           group = rep(1, length(name)),
                           size = rep(15, length(name)))
    forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.4)
  })
}



shinyApp(ui, server)