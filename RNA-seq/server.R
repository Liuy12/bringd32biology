# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {
  source('www/R/Startanalysis.R', local = TRUE)
  source('www/R/myheatmap.R', local =TRUE)
  options(shiny.maxRequestSize=2000*1024^2) 
  options(pandoc.stack.size = '2048m')
  outputOptions(output, "Heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "Density", suspendWhenHidden = FALSE)
  outputOptions(output, "ScatterPlot", suspendWhenHidden = FALSE)
  outputOptions(output, "Boxplot", suspendWhenHidden = FALSE)
  outputOptions(output, "forceNetworkGene", suspendWhenHidden = FALSE)
  outputOptions(output, "MAplot", suspendWhenHidden = FALSE)
  outputOptions(output, "DEheatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "DispersionPlot", suspendWhenHidden = FALSE)
  outputOptions(output, "Spikeinsqc", suspendWhenHidden = FALSE)
  outputOptions(output, "Table", suspendWhenHidden = FALSE)
#  outputOptions(output, "PrincipalComponent2d", suspendWhenHidden = FALSE)
#  outputOptions(output, "PrincipalComponent3d", suspendWhenHidden = FALSE)
  outputOptions(output, "DEtable", suspendWhenHidden = FALSE)
  outputOptions(output, "HVGtable", suspendWhenHidden = FALSE)
  outputOptions(output, "HVGheatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "HVGplot", suspendWhenHidden = FALSE)
  
  
  output$totalvisits <- renderValueBox({
    library(rga)
    rga.open(instance = "ga", where = 'ga.rga')
    if (ga$isTokenExpired()){
      ga$refreshToken()
    }
    id <- "108354935"
    today <- Sys.Date()
    myresults <- ga$getData(id, start.date="2015-09-13", end.date=today, metrics = "ga:visits", dimensions = "ga:month")
    valueBox(sum(myresults$visits), 'Total page visits', icon = icon('eye'))
  })
  
  output$thismonthvisits <- renderValueBox({
    library(rga)
    rga.open(instance = "ga", where = 'ga.rga')
    if (ga$isTokenExpired()){
      ga$refreshToken()
    }
    id <- "108354935"
    today <- Sys.Date()
    myresults <- ga$getData(id, start.date=today-30, end.date=today, metrics = "ga:visits", dimensions = "ga:month")
    valueBox(sum(myresults$visits), 'Page visits last month', icon = icon('eye'), color = 'blue')
  })
})


