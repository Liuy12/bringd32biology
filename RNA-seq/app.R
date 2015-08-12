library(data.table)
library(shiny)
library(d3heatmap)
library(networkD3)
library(DT)
library(rCharts)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidRow(column(12, tags$header(strong(HTML("<p>RNA-seq widget: Bring <span style='color: red;'>D3</span> visualization to RNA-seq!")), style = "font-size: 50px; 
                                  background-color: #F0FFFF;"))),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV/TXT File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("Table")),
        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Density", showOutput("Density", "nvd3")), 
        tabPanel("Scatter Plot", sidebarLayout(
          sidebarPanel(
            textInput("text1", label = h5("Enter first sample name"), value = "Enter text..."),
            verbatimTextOutput("value1"),
            tags$hr(),
            textInput("text2", label = h5("Enter second sample name"), value = "Enter text..."),
            verbatimTextOutput("value2")
          ),
          mainPanel(showOutput("ScatterPlot", "polycharts")))
        ),
        tabPanel("Boxplot", plotOutput("Boxplot")),
        tabPanel("Principal Component", forceNetworkOutput("PrincipalComponent")),
        tabPanel("forceNetworkGene", forceNetworkOutput("forceNetworkGene"))
        )
    )
  )
)

server <- function(input, output) {
  # generate heatmap using D3heatmap
  dataMat <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    fread(inFile$datapath, data.table=F)
  })
  
  output$Table <- renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    colnames(dataMat()) <- paste('S', 1:ncol(dataMat()), sep='')
    datatable(dataMat(), options = list(pageLength = 5))
  })
  
  output$Heatmap <- renderD3heatmap({
    if (is.null(input$file1))
      return(NULL)
    colnames(dataMat()) <- paste('S', 1:ncol(dataMat()), sep='')
    d3heatmap(dataMat(), scale="row", colors=colorRampPalette(c("blue","white","red"))(1000))
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
  
  output$PrincipalComponent <- renderForceNetwork({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    MisLinks <- data.frame(source = rep(0:(ncol(dataMat)-1), each = ncol(dataMat)),
                           target = rep(0:(ncol(dataMat)-1), times = ncol(dataMat)),
                           value = c(cor(dataMat, method = 'spearman')))
    MisNodes <- data.frame(name = paste('Sample', 1:ncol(dataMat), sep=''),
                           group = rep(1, ncol(dataMat)),
                           size = rep(15, ncol(dataMat)))
    forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.4)
  })
}

shinyApp(ui, server)