# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {
  source('www/R/Startanalysis.R', local = TRUE)
  
  outputOptions(output, "Heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "Density", suspendWhenHidden = FALSE)
  outputOptions(output, "ScatterPlot", suspendWhenHidden = FALSE)
  outputOptions(output, "Boxplot", suspendWhenHidden = FALSE)
  outputOptions(output, "forceNetworkGene", suspendWhenHidden = FALSE)
  outputOptions(output, "MAplot", suspendWhenHidden = FALSE)
  outputOptions(output, "DEheatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "DispersionPlot", suspendWhenHidden = FALSE)
  
  output$totalvisits <- renderValueBox({
    load("www/ga.rga")
    if (ga$isTokenExpired()){
      ga$refreshToken()
    }
    id <- "108354935"
    today <- Sys.Date()
    myresults <- ga$getData(id, start.date="2015-09-13", end.date=today, metrics = "ga:visits", dimensions = "ga:month")
    valueBox(sum(myresults$visits), 'Total page visits', icon = icon('eye'))
  })
  
  output$thismonthvisits <- renderValueBox({
    load("www/ga.rga")
    if (ga$isTokenExpired()){
      ga$refreshToken()
    }
    id <- "108354935"
    today <- Sys.Date()
    myresults <- ga$getData(id, start.date=today-30, end.date=today, metrics = "ga:visits", dimensions = "ga:month")
    valueBox(sum(myresults$visits), 'Page visits last month', icon = icon('eye'), color = 'blue')
  })
})


