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
      selectizeInput(
        "DEmethod", 
        label = 'Please select a method for DE analysis',
        choices = c('XBSeq', 'DESeq', 'DESeq2', 'edgeR', 'edgeR-robust', 'limma-voom'),
        options = list(placeholder = 'select a method below',
                       onInitialize = I('function() { this.setValue(""); }'))
        ),
      verbatimTextOutput("value_DE"),
      fileInput(
        'file_obs', 'Choose CSV/TXT File for RNA-seq', accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')
        ),
      conditionalPanel(
        condition = "input.DEmethod == 'XBSeq'",
        fileInput(
          'file_bg', 'Choose CSV/TXT File for RNA-seq (bg), required if you choose XBSeq', 
          accept=c('text/csv',
                   'text/comma-separated-values,text/plain', 
                   '.csv')
        )
      ),
      fileInput('file_design', 
                'Choose CSV/TXT File for experiment design',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')
                ),
      conditionalPanel(
        condition = "input.DEmethod == 'DESeq' || input.DEmethod == 'XBSeq'",
        selectizeInput("SCVmethod", 
                       label = "Please select a method to estimate dispersion", 
                       choices =c('pooled', 'per-condition', 'blind'),
                       options = list(placeholder = 'select a method below',
                                      onInitialize = I('function() { this.setValue(""); }'))
                       ),
        verbatimTextOutput("SCVmethod"),
        selectizeInput("SharingMode",
                       label = "Please select a method for sharing mode",
                       choices = c('maximum', 'fit-only', 'gene-est-only'),
                       options = list(placeholder = 'select a method below',
                                      onInitialize = I('function() { this.setValue(""); }'))
                       ),
        verbatimTextOutput("SharingMode"),
        selectizeInput("fitType",
                       label = "Please select a method for fitType",
                       choices = c('local', 'parametric'),
                       options = list(placeholder = 'select a method below',
                                      onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("fitType"),        
        conditionalPanel(
          condition = "input.DEmethod == 'XBSeq'",
          selectizeInput("ParamEst", 
                         label = "Please select a method to estimate distribution parameters", 
                         choices =c('Non-parametric' = 'NP', 
                                    'Maximum liklihood estimation' = 'MLE'),
                         options = list(placeholder = 'select a method below',
                                        onInitialize = I('function() { this.setValue(""); }'))
          ),
          verbatimTextOutput("ParamEst")          
        )
      ),
      conditionalPanel(
        condition = "input.DEmethod == 'DESeq2'",
        selectizeInput("fitType_DESeq2", 
                       label = "Please select a method for fit type", 
                       choices =c('local', 'parametric', 'mean'),
                       options = list(placeholder = 'select a method below',
                                      onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("fitType_DESeq2"),
        selectizeInput("Test",
                       label = "Please select a method for statistical test",
                       choices = c('Wald test' = 'Wald',
                                   'Log ratio test' = 'LRT'),
                       options = list(placeholder = 'select a method below',
                                      onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("Test"),
        selectizeInput("cooksCutoff",
                       label = "Please select a value for cooks distance cutoff",
                       choices = c('FALSE',
                                   0.99,
                                   0.95,
                                   0.90),
                       options = list(placeholder = 'select a value below',
                                      onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("cooksCutoff")
      ),
      conditionalPanel(
        condition = "input.DEmethod == 'edgeR-robust'",
        selectizeInput("residualType", 
                       label = "Please select a method for calculating residuals", 
                       choices =c("pearson", "deviance", "anscombe"),
                       options = list(placeholder = 'select a method below',
                                      onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("residualType")
      ),
      selectizeInput("padjust", 
                     label = "Please select a method for adjusting p values", 
                     choices =c("Benj&Hoch" = "BH", 
                                "bonferroni", "none"),
                     options = list(placeholder = 'select a method below',
                                    onInitialize = I('function() { this.setValue(""); }'))
      ),
      verbatimTextOutput("padjust"),
      selectizeInput("pcutoff", 
                     label = "Please set a cutoff of p values for DE genes", 
                     choices =c(0.001, 0.01, 0.05, 0.1, 0.2),
                     options = list(placeholder = 'select a value below',
                                    onInitialize = I('function() { this.setValue(""); }'))
      ),
      verbatimTextOutput("pcutoff"),
      selectizeInput("fccutoff", 
                     label = "Please set a cutoff of fold change for DE genes", 
                     choices =c(1.5, 2, 2.5, 3, 5),
                     options = list(placeholder = 'select a value below',
                                    onInitialize = I('function() { this.setValue(""); }'))
      ),
      verbatimTextOutput("fccutoff")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("Table")),
        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Density", showOutput("Density", "nvd3")), 
        tabPanel("Scatter Plot", sidebarLayout(
          sidebarPanel(
            textInput("text_S1", label = h5("Enter first sample name (For example, S1)"), value = "Enter text..."),
            verbatimTextOutput("value_S1"),
            textInput("text_S2", label = h5("Enter second sample name (For example, S2)"), value = "Enter text..."),
            verbatimTextOutput("value_S2")
          ),
          mainPanel(showOutput("ScatterPlot", "polycharts")))
        ),
        tabPanel("Boxplot", plotOutput("Boxplot")),
        tabPanel("Principal Component", sidebarLayout(
          sidebarPanel(
            selectizeInput("cvCutoff", label = 'Please select a cutoff for cv (coefficient of variation)',choices = c(0.1, 0.3, 0.5)),
            verbatimTextOutput("value_cvcutoff"),
            selectizeInput("clusterMethod", label = 'Please select a method for clustering (pca or mds)',choices = c('pca', 'mds')),
            verbatimTextOutput("value_clutermethod")
            ),
          mainPanel(showOutput("PrincipalComponent", "nvd3")))
          ),
        tabPanel("Gene interaction network", sidebarLayout(
          sidebarPanel(
            sliderInput("Exprscut", "Expression level cutoff", 
                        min=0, max=100, step = 10, value=10),
            verbatimTextOutput("value_Exprscut"),
            sliderInput("Corrcut", "Correlation cutoff", 
                        min=0, max=1, step = 0.1, value=0.7),
            verbatimTextOutput("value_Corrcut")
          ),
          mainPanel(forceNetworkOutput("forceNetworkGene")))
        )
    )
  )
))




server <- function(input, output, session) {
  # generate heatmap using D3heatmap
  dataMat <- reactive({
    inFile <- input$file_obs
    if (is.null(inFile))
      return(NULL)
    fread(inFile$datapath, data.table=F)
  })
  
  design <- reactive({
    inFile <- input$file_design
    if (is.null(inFile))
      return(NULL)
    fread(inFile$datapath, data.table=F)
  })
  
  output$value_DE <- renderPrint({input$DEmethod})
  
  output$SCVmethod <- renderPrint({input$SCVmethod})
  
  output$SharingMode <- renderPrint({input$SharingMode})
  
  output$fitType <- renderPrint({input$fitType})
  
  output$ParamEst <- renderPrint({input$ParamEst})
  
  output$fitType_DESeq2 <- renderPrint({input$fitType_DESeq2})
  
  output$Test <- renderPrint({input$Test})
  
  output$cooksCutoff <- renderPrint({input$cooksCutoff})
  
  output$residualType <- renderPrint({input$residualType})
  
  output$padjust <- renderPrint({input$padjust})
  
  output$pcutoff <- renderPrint({input$pcutoff})
  
  output$fccutoff <- renderPrint({input$fccutoff})
  
  output$Table <- renderDataTable({
    if (is.null(input$file_obs))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    datatable(dataMat, options = list(pageLength = 5))
  })
  
  output$Heatmap <- renderD3heatmap({
    if (is.null(input$file_obs))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    d3heatmap(dataMat, scale="row", colors=colorRampPalette(c("blue","white","red"))(1000))
  })
  
  output$Density <- renderChart({
    if (is.null(input$file_obs))
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
  
  output$value_S1 <- renderPrint({input$text_S1})
  
  output$value_S2 <- renderPrint({input$text_S2})
  
  output$ScatterPlot <- renderChart({
    if (is.null(input$file_obs))
      return(NULL)
    dataMat <- dataMat()
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    rp <- rPlot(input$text_S1, input$text_S2, data = dataMat, type = 'point')
    rp$addParams(dom = "ScatterPlot")
    return(rp)
  })
  
  output$value_cvcutoff <- renderPrint({input$cvCutoff})
  
  output$value_clustermethod <- renderPrint({input$clusterMethod})
  
  output$PrincipalComponent <- renderChart({
    if (is.null(input$file_obs))
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
  
  output$value_Exprscut <- renderPrint({input$Exprscut})
  
  output$value_Corrcut <- renderPrint({input$Corrcut})
  
  output$forceNetworkGene <- renderForceNetwork({
    if (is.null(input$file_obs))
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