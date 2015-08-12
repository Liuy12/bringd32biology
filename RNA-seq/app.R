library(data.table)
library(shiny)
library(d3heatmap)
library(networkD3)
library(DT)
library(metricsgraphics)

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
        tabPanel("Table", dataTableOutput("Table")),
        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Density", metricsgraphicsOutput("Density")), 
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
  output$Table <- renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    datatable(dataMat(), options = list(pageLength = 5))
  })
  output$Heatmap <- renderD3heatmap({
    if (is.null(input$file1))
      return(NULL)
    d3heatmap(dataMat(), scale="row", colors=colorRampPalette(c("blue","white","red"))(1000))
  })
  output$Density <- renderMetricsgraphics({
    if (is.null(input$file1))
      return(NULL)
    dataMat <- dataMat()
    denStat <- density(dataMat[,1], from = min(dataMat), to = max(dataMat))
    denStat <- data.frame(x = denStat$x,
                          y = denStat$y)
    denStat1 <- sapply(2:ncol(dataMat), function(i){
      preset <- density(dataMat[,i], from = min(dataMat), to = max(dataMat))
      preset$y
    })
    denStat <- cbind(denStat, denStat1)
    colnames(denStat) <- c('Exprs', paste('S',1:ncol(dataMat),sep = ''))
    denPlot <- mjs_plot(denStat, Exprs, S1, decimals = 6) %>% 
      mjs_line() %>%
      mjs_add_line(S2) %>%
      mjs_add_line(S3) %>%
      mjs_add_line(S4) %>%
      mjs_add_line(S5) %>%
      mjs_add_line(S6)
    denPlot <- denPlot %>% 
      mjs_labs(x_label = 'Expression', y_label = 'Density') %>%
      mjs_add_legend(legend=colnames(dataMat))
    denPlot
  })
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
