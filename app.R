library(data.table)
library(shiny)
library(d3heatmap)

ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                 'text/comma-separated-values,text/plain', 
                 '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    mainPanel(
      d3heatmapOutput('contents')
    )
  )
)

server <- function(input, output) {
  output$contents <- renderD3heatmap({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    temp <- fread(inFile$datapath, header=input$header, sep=input$sep, data.table=F)
    d3heatmap(temp, scale="row", colors="Blues")
  })
}

shinyApp(ui, server)