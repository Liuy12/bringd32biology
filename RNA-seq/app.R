library(data.table)
library(shiny)
library(d3heatmap)
library(networkD3)

ui <- fluidPage(
  titlePanel("RNA-seq"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV/TXT File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Density", plotOutput("Density")), 
        tabPanel("Boxplot", plotOutput("Boxplot")),
        tabPanel("SampleCorrelation", plotOutput("SampleCorrelation")),
        tabPanel("forceNetworkSample", forceNetworkOutput("forceNetworkSample")),
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
  output$Heatmap <- renderD3heatmap({
    if (is.null(input$file1))
      return(NULL)
    d3heatmap(dataMat(), scale="row", colors=colorRampPalette(c("blue","white","red"))(1000))
  })
  # generate network using networkD3
  output$forceNetworkSample <- renderForceNetwork({
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
