output$InputBox <- renderUI(
    fluidRow(
      column(width = 12, 
             box(
               title = "File input", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
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
               )
             ), 
             box(
               title = "Options for DE method", status = "info", solidHeader = TRUE,
               collapsible = TRUE,
               conditionalPanel(
                 condition = "input.DEmethod == 'DESeq' | input.DEmethod == 'XBSeq'",
                 selectizeInput("SCVmethod", 
                                label = "Please select a method to estimate dispersion", 
                                choices =c('pooled', 'per-condition', 'blind'),
                                selected = 'pooled'
                 ),
                 verbatimTextOutput("SCVmethod"),
                 selectizeInput("SharingMode",
                                label = "Please select a method for sharing mode",
                                choices = c('maximum', 'fit-only', 'gene-est-only'),
                                selected = 'maximum'
                 ),
                 verbatimTextOutput("SharingMode"),
                 selectizeInput("fitType",
                                label = "Please select a method for fitType",
                                choices = c('local', 'parametric'),
                                selected = 'local'
                 ),
                 verbatimTextOutput("fitType"),
                 conditionalPanel(
                   condition = "input.DEmethod == 'XBSeq'",
                   selectizeInput("ParamEst", 
                                  label = "Please select a method to estimate distribution parameters", 
                                  choices =c('Non-parametric' = 'NP', 
                                             'Maximum liklihood estimation' = 'MLE'),
                                  selected = 'NP'
                   ),
                   verbatimTextOutput("ParamEst")
                 )
               ),
               conditionalPanel(
                 condition = "input.DEmethod == 'DESeq2'",
                 selectizeInput("fitType_DESeq2", 
                                label = "Please select a method for fit type", 
                                choices =c('local', 'parametric', 'mean'),
                                selected = 'local'
                 ),
                 verbatimTextOutput("fitType_DESeq2"),
                 selectizeInput("Test",
                                label = "Please select a method for statistical test",
                                choices = c('Wald test' = 'Wald',
                                            'Log ratio test' = 'LRT'),
                                selected = 'Wald'
                 ),
                 verbatimTextOutput("Test"),
                 selectizeInput("cooksCutoff",
                                label = "Please choose either to turn on or off cooks distance cutoff",
                                choices = c('on',
                                            'off'),
                                selected = 'off'
                 ),
                 verbatimTextOutput("cooksCutoff")
               )
#                conditionalPanel(
#                  condition = "input.DEmethod == 'edgeR-robust'",
#                  selectizeInput("residualType", 
#                                 label = "Please select a method for calculating residuals", 
#                                 choices =c("pearson", "deviance", "anscombe"),
#                                 selected = 'pearson'
#                  ),
#                  verbatimTextOutput("residualType")
#                )
             )
             ),
      column(width = 12, 
             box(
               title = "Criteria for DE genes", status = "success", solidHeader = TRUE,
               collapsible = TRUE,
               selectizeInput("padjust", 
                              label = "Please select a method for adjusting p values", 
                              choices =c("Benj&Hoch" = "BH", 
                                         "bonferroni", "none"),
                              selected = 'BH'
               ),
               verbatimTextOutput("padjust"),
               selectizeInput("pcutoff", 
                              label = "Please set a cutoff of p values for DE genes", 
                              choices =c(0.001, 0.01, 0.05, 0.1, 0.2),
                              selected = 0.05
               ),
               verbatimTextOutput("pcutoff"),
               selectizeInput("fccutoff", 
                              label = "Please set a cutoff of fold change for DE genes", 
                              choices =c(1.5, 2, 2.5, 3, 5),
                              selected = 2
               ),
               verbatimTextOutput("fccutoff"),
               numericInput("log2bmcutoff", label = "Please set a cutoff for log2 expression intensity (Usually can be determined from density plot)", 
                            value = 5, min = 1
               ),
               verbatimTextOutput("log2bmcutoff"),
               actionButton('DEstart', label = 'Start analysis!'),
               textOutput("DEstart")
             )
             )
    )
  )

output$Chartpage <- renderUI({
  fluidPage(
      tabBox(width = 12,
        title = tagList(shiny::icon("tag"), "Quality control"),
        id = "QCtab",
        tabPanel("Table", dataTableOutput("Table")),
        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Kernel Density Estimation", showOutput("Density", "nvd3")), 
        tabPanel("Scatter Plot", fluidPage(
          fluidRow(
            column(4, offset = 1, textInput("text_S1", label = "Enter first sample name (For example, S1)", 
                                            value = "S1"
            )),
            column(4,offset = 2, textInput("text_S2", label = "Enter second sample name (For example, S2)", 
                                           value = "S2"
            ))
          ), 
          fluidRow(
            column(4, offset = 1, verbatimTextOutput("value_S1")),
            column(4, offset = 2, verbatimTextOutput("value_S2"))
          ),
          hr(),
          showOutput("ScatterPlot", "highcharts")
          )),
        tabPanel("Boxplot", showOutput("Boxplot", "highcharts"))
      ),
      tabBox(title = tagList(shiny::icon("tag"), 'Gene/Sample relationship'), id = 'relationTab',
             width = 12,
             tabPanel("Principal Component",
               fluidRow(
                 column(4, offset = 1, selectizeInput("cvCutoff", 
                                                      label = 'Please select a cutoff for cv (coefficient of variation)',
                                                      choices = c(0.1, 0.3, 0.5))
                 ),
                 column(4,offset = 2, selectizeInput("clusterMethod", 
                                                     label = 'Please select a method for clustering (pca or mds)',
                                                     choices = c('pca', 'mds'))
                 )),
               fluidRow(
                 column(4, offset = 1, verbatimTextOutput("value_cvcutoff")),
                 column(4, offset = 2, verbatimTextOutput("value_clustermethod"))
               ),
               hr(),
               fluidRow(
                 column(12, showOutput("PrincipalComponent", "dimple"))
                 )
               ),
             tabPanel("Gene interaction network", fluidPage(
               fluidRow(
                 column(4, offset = 1, sliderInput("Exprscut", "Expression level cutoff", 
                                                   min=0, max=20, step = 2, value=8
                 )),
                 column(4,offset = 2, sliderInput("Corrcut", "Correlation cutoff", 
                                                  min=0, max=1, step = 0.1, value=0.9)
                 )),
               fluidRow(
                 column(4, offset = 1, verbatimTextOutput("value_Exprscut")),
                 column(4, offset = 2, verbatimTextOutput("value_Corrcut"))
               )),
               hr(),
               forceNetworkOutput("forceNetworkGene")
             )
      ),
      if(input$DEmethod != 'limma-voom')
        tabBox(
          title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
          id = 'DEanalysis', width = 12, 
          tabPanel("DE Table", dataTableOutput('DEtable'), style = "max-width:50%"),
          tabPanel("MAplot", metricsgraphicsOutput("MAplot")),
          tabPanel("DE Heatmap", d3heatmapOutput('DEheatmap')),
          tabPanel("Dispersion plot", showOutput("DispersionPlot", "polycharts"))
          #         conditionalPanel(condition = "input.DEmethod == 'XBSeq'",
          #                          tabPanel("XBSeq plot", showOutput("XBSeqPlot", "nvd3")))
        )
      else
        tabBox(
          title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
          id = 'DEanalysis', width = 12, 
          tabPanel("DE Table", dataTableOutput('DEtable'), style = "max-width:50%"),
          tabPanel("MAplot", metricsgraphicsOutput("MAplot")),
          tabPanel("DE Heatmap", d3heatmapOutput('DEheatmap'))
          #         conditionalPanel(condition = "input.DEmethod == 'XBSeq'",
          #                          tabPanel("XBSeq plot", showOutput("XBSeqPlot", "nvd3")))
        ),
    box(title = 'File exports', collapsible = T, status = 'success', width = 12,
    downloadButton('StartDownload', label = "Download")
)
  )
})


output$StartDownload <- downloadHandler(
  filename <- 'Report.zip',
  content <- function(file) {
    file.copy(c('./forceNet.html', './MAplot.html', './datatable.html', './DEdatatable.html'), 'www/report/htmlFiles')
    slidify('www/report/Report.Rmd')
    zip(file, files = c('www/report/'))
  },
  contentType = 'application/zip'
)

StartMessage <- eventReactive(input$DEstart, {
  "Please wait, this might take a while"
})

output$DEstart <- renderText({
  StartMessage()
})

design <- reactive({
  inFile <- input$file_design
  if (is.null(inFile))
    return(NULL)
  design <- fread(inFile$datapath, data.table = F)
  design[[1]]
})

dataComb <- eventReactive(input$DEstart, {
  inFile <- input$file_obs
  if (is.null(inFile))
    return(NULL)
  data_obs <- fread(inFile$datapath, data.table = F)
  rownames(data_obs) <- data_obs[,1]
  data_obs <- data_obs[,-1]
  group <- design()
  group <- as.factor(group)
  if (input$DEmethod == 'XBSeq') {
    data_bg <- fread(input$file_bg$datapath, data.table = F)
    rownames(data_bg) <- data_bg[,1]
    data_bg <- data_bg[,-1]
    XBSeq_pfun(data_obs, data_bg, group, disp_method = input$SCVmethod, 
               sharing_mode = input$SharingMode, fit_type = input$fitType,
               paraMethod = input$ParamEst)
  }
  else if (input$DEmethod == 'DESeq') {
    DESeq_pfun(data_obs, group, disp_method = input$SCVmethod,
               sharing_mode = input$SharingMode, 
               fit_type = input$fitType)
  }
  else if (input$DEmethod == 'DESeq2') {
    DESeq2_pfun(
      data_obs, group, cookcutoff = input$cooksCutoff,
      fittype = input$fitType_DESeq2, test = input$Test
    )
  }
  else if (input$DEmethod == 'edgeR') {
    edgeR.pfun(data_obs, group, model.matrix(~group))
  }
  else if (input$DEmethod == 'edgeR-robust') {
    edgeR_robust.pfun(data_obs, group, model.matrix(~group))
  }
  else if (input$DEmethod == 'limma-voom') {
    limma_voom.pfun(data_obs, group, model.matrix(~group))
  }
})

output$value_DE <- renderPrint({
  input$DEmethod
})

output$SCVmethod <- renderPrint({
  input$SCVmethod
})

output$SharingMode <- renderPrint({
  input$SharingMode
})

output$fitType <- renderPrint({
  input$fitType
})

output$ParamEst <- renderPrint({
  input$ParamEst
})

output$fitType_DESeq2 <- renderPrint({
  input$fitType_DESeq2
})

output$Test <- renderPrint({
  input$Test
})

output$cooksCutoff <- renderPrint({
  input$cooksCutoff
})

output$residualType <- renderPrint({
  input$residualType
})

output$padjust <- renderPrint({
  input$padjust
})

output$pcutoff <- renderPrint({
  as.numeric(input$pcutoff)
})

output$fccutoff <- renderPrint({
  as.numeric(input$fccutoff)
})

output$log2bmcutoff <- renderPrint({
  input$log2bmcutoff
})

output$Table <- renderDataTable({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
  temp <- datatable(dataMat, options = list(pageLength = 5))
  saveWidget(temp, 'datatable.html')
  temp
})

output$Heatmap <- renderD3heatmap({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
  mean_gene <- apply(dataMat, 1, mean)
  var_gene <- apply(dataMat, 1, var)
  index <- which(log2(mean_gene) > input$log2bmcutoff)
  dataMat1 <- dataMat[order(var_gene[index]),]
  if (length(index) < 100)
    return(NULL)
  d3heatmap(dataMat1[1:100,], scale = "row", colors = colorRampPalette(c("blue","white","red"))(1000))
})

output$Density <- renderChart({
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  denStat <-
    density(dataMat[,1], from = min(dataMat), to = max(dataMat))
  denStat <- data.frame(x = denStat$x,
                        y = denStat$y)
  denStat1 <- sapply(1:ncol(dataMat), function(i) {
    preset <-
      density(dataMat[,i], from = min(dataMat), to = max(dataMat))
    preset$y
  })
  colnames(denStat1) <- c(paste('S',1:ncol(dataMat),sep = ''))
  denStat1 <- stack(as.data.frame(denStat1))
  denStat <-
    cbind(rep(denStat[,1], times = ncol(dataMat)), denStat1)
  colnames(denStat) <- c('Exprs', 'Density', 'ind')
  np <-
    nPlot(Density ~ Exprs, group = 'ind', data = denStat, type = 'lineChart')
  np$addParams(dom = "Density")
  np$chart(useInteractiveGuideline = TRUE)
  if(input$DEmethod == 'edgeR' | input$DEmethod == 'edgeR-robust')
    xlab <- 'Log2 normalized cpm (counts per million)'
  else
    xlab <- 'Log2 normalized intensity'
  np$xAxis(axisLabel = xlab)
  np$yAxis(axisLabel = 'Density')
  np$save('www/report/htmlFiles/density.html', standalone = TRUE)
  return(np)
})

output$value_S1 <- renderPrint({
  input$text_S1
})

output$value_S2 <- renderPrint({
  input$text_S2
})

output$ScatterPlot <- renderChart({
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
  dataMat <- as.data.frame(dataMat)
  dataMat$GeneName <- rownames(dataMat)
  hp <-
    hPlot(
      x = input$text_S1, y = input$text_S2, data = dataMat, type = 'scatter', radius = 3
    )
  hp$addParams(dom = "ScatterPlot")
  hp$colors('black')
#       hp$tooltip(useHTML = T, formatter = "#! function() { return 'x: '     + this.point.x +
#                                                   'y: '    + this.point.y  +
#                                                   'name: '  + this.point.GeneName; } !#")
#   hp <- Highcharts$new()
#   hp$chart(type = "scatter")
#   temp <- input$text_S1
#   temp1 <- input$text_S2
#   dataMat$y <- dataMat$S1
#   dataMat$x <- dataMat$S2
#   hp$series(data = toJSONArray2(dataMat, json = F), name = "Observation")
#   hp$tooltip(useHTML = T, formatter = "#! function() { return 'x:' + this.point.x + '<br>' + 'y:' + this.point.y + '<br>' + 'Genename:' + this.point.GeneName ; } !#")
#   hp$colors('black')
#   hp$addParams(dom = "ScatterPlot")
  hp$save('www/report/htmlFiles/Scatterplot.html', standalone = TRUE)
  hp
})

output$value_cvcutoff <- renderPrint({
  input$cvCutoff
})

output$value_clustermethod <-
  renderPrint({
    input$clusterMethod
  })

output$Boxplot <- renderChart({
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
  bwstats <- setNames(as.data.frame(boxplot(dataMat, plot = F)$stats),
                      nm = NULL)
  hp <- Highcharts$new()
  hp$set(series = list(list(name = 'Observations',
                            data = bwstats)))
  hp$xAxis(categories = colnames(dataMat),
           title = list(text = 'Sample'))
  if(input$DEmethod == 'edgeR' | input$DEmethod == 'edgeR-robust')
    ylab <- 'Log2 normalized cpm (counts per million)'
  else
    ylab <- 'Log2 normalized intensity'
  hp$yAxis(title = list(text = ylab))
  hp$chart(type = 'boxplot')
  hp$addParams(dom = "Boxplot")
  hp$save('www/report/htmlFiles/Boxplot.html', standalone = TRUE)
  hp
})

output$PrincipalComponent <- renderChart({
  colors <-
    c(
      '#00FFFF', '#FFE4C4', '#D2691E', '#6495ED', '#9932CC', '#8B0000',
      '#FF00FF', '#FFD700'
    )
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  design <- design()
  colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
  cvcutoff <- input$cvCutoff
  clusterMethod <- input$clusterMethod
  cv.gene <- apply(dataMat, 1, function(x)
    sd(x) / mean(x))
  dataMat <- dataMat[which(cv.gene > cvcutoff),]
  dataMat <- scale(dataMat)
  if (clusterMethod == 'mds') {
    dd <- dist(t(dataMat))
    mds.result <- cmdscale(dd, k = 2, eig = TRUE)
    ppoints <- mds.result$points
  }
  else{
    pca.result <- prcomp(t(dataMat))
    ppoints <- pca.result$x[,1:2]
  }
  ppoints <-
    cbind(ppoints, design, paste('S', 1:ncol(dataMat), sep = ''))
  colnames(ppoints) <- c('PC1', 'PC2', 'Design', 'Samplename')
  dp <-
    dPlot(
      PC2 ~ PC1, data = as.data.frame(ppoints), 
      groups = c('Samplename', 'Design'), type = 'bubble')
  dp$xAxis(type = 'addMeasureAxis')
  dp$addParams(dom = "PrincipalComponent")
  dp$save('www/report/htmlFiles/pcaplot.html', cdn = TRUE)
  dp
})

output$value_Exprscut <- renderPrint({
  input$Exprscut
})

output$value_Corrcut <- renderPrint({
  input$Corrcut
})

output$forceNetworkGene <- renderForceNetwork({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
  mean.gene <- apply(dataMat, 1, mean)
  dataMat <- dataMat[mean.gene > input$Exprscut,]
  if (length(dataMat) == 0 |
      length(dataMat) == ncol(dataComb[[2]]))
    return(NULL)
  else
  {
    MisLinks <-
      data.frame(
        source = rep(0:(nrow(dataMat) - 1), each = nrow(dataMat)),
        target = rep(0:(nrow(dataMat) - 1), times = nrow(dataMat)),
        value = c(cor(t(dataMat), method = 'spearman'))
      )
    index <- which(abs(MisLinks$value) > input$Corrcut)
    MisLinks <- MisLinks[index,]
    name <- unique(rep(rownames(dataMat),
                       each = nrow(dataMat))[index])
    MisNodes <- data.frame(
      name = name,
      group = rep(1, length(name)),
      size = rep(15, length(name))
    )
    forceNet <- forceNetwork(
      Links = MisLinks, Nodes = MisNodes, Source = "source",
      Target = "target", Value = "value", NodeID = "name",
      Group = "group", opacity = 0.4
    )
    saveWidget(forceNet, 'forceNet.html')
    return(forceNet)
  }
})

output$DEtable <- renderDataTable({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  dataMat <- log2(dataComb[[2]])
  dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
  dataMat <- as.data.frame(dataMat)
  dataMat1 <- dataComb[[4]]
  p_adjust1 <- p.adjust(dataMat1[,3], method = input$padjust)
  p_adjust1[is.na(p_adjust1)] <- 1
  DE_index <-
    which(
      log2(dataMat1[,1] + 1) > as.numeric(input$log2bmcutoff) &
        abs(dataMat1[,2]) > log2(as.numeric(input$fccutoff)) &
        p_adjust1 < as.numeric(input$pcutoff)
    )
  if (length(DE_index) == 0)
    return(datatable(data.frame(), options = list(pageLength = 5)))
  dataMat2 <-
    cbind(dataMat[DE_index,], dataMat1[DE_index,2], p_adjust1[DE_index])
  colnames(dataMat2) <-
    c(paste('S', 1:ncol(dataMat), sep = ''), 'Log2 fold change', 'p adjusted value')
  temp <- datatable(dataMat2, options = list(pageLength = 5))
  saveWidget(temp, 'DEdatatable.html')
  temp
})

output$MAplot <- renderMetricsgraphics({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  dataMat <- dataComb[[4]]
  colnames(dataMat) <- c('baseMean', 'log2FoldChange', 'p_adjust')
  p_adjust1 <- p.adjust(dataMat[,3], method = input$padjust)
  p_adjust1[is.na(p_adjust1)] <- 1
  col <-
    with(
      data = dataMat, ifelse(
        log2(baseMean + 1) > as.numeric(input$log2bmcutoff) &
          abs(log2FoldChange) > log2(as.numeric(input$fccutoff)) &
          p_adjust1 < as.numeric(input$pcutoff),
        "DE", "Not DE"
      )
    )
  dataout <- cbind(dataComb[[2]], dataMat[,2], p_adjust1)
  colnames(dataout) <- c(colnames(dataComb[[2]]), 'Log2 Fold change', 'p value')
  write.csv(dataout, 'www/report/TestStat.csv', quote = F)
  write.csv(dataout[col=='DE',], 'www/report/DEstat.csv', quote = F)
  dataMat <- cbind(Genename = rownames(dataMat), dataMat, col)
  dataMat$baseMean <- log2(dataMat$baseMean + 1)
  if(input$DEmethod == 'edgeR' | input$DEmethod == 'edgeR-robust')
    xlab <- 'Log2 normalized cpm (counts per million)'
  else
    xlab <- 'Log2 normalized intensity'
  if(length(unique(col)) == 1)
    color_rg <- 'grey32'
  else
    color_rg <- c('red', 'grey32')
  mp <-
    mjs_plot(dataMat, baseMean, log2FoldChange, decimals = 6) %>%
    mjs_point(
      color_accessor = col, color_range = color_rg, color_type = "category", x_rug =
        TRUE, y_rug = TRUE
    ) %>%
    mjs_add_baseline(y_value = 0, label = 'baseline') %>%
    mjs_labs(x_label = xlab, y_label = "Log2 fold change")
  saveWidget(mp, file = 'MAplot.html')
  mp
})

output$DispersionPlot <- renderChart3({
  dataComb <- dataComb()
  Dispersion <- dataComb[[3]]
  Dispersion$baseMean <- dataComb[[4]][,1] + 1
  if(input$DEmethod == 'DESeq2'){
    Dispersion1 <- stack(Dispersion[,c(1,3)])
    colnames(Dispersion1) <- c('Disp', 'Type')
    Dispersion1$baseMean <- rep(Dispersion$baseMean, times = 2)
    Dispersion1$Dispfit <- rep(Dispersion$dispFit, times = 2)
    Dispersion1$GeneName <- rep(rownames(dataComb[[2]]), times = 2)
    rp <-
      rPlot(
        Disp ~ baseMean, data = Dispersion1, color = 'Type', type = 'point', size = list(const = 2),
        tooltip = "#!function(item){ return 'x: ' + item.baseMean + '\\n' +
        ' y: ' + item.Disp + '\\n' + ' GeneName: ' + item.GeneName  + '\\n' + 'Type: ' + item.Type}!#"
      )
    rp$layer(
      y = 'Dispfit', type = 'line', color = list(const = 'red'),
      copy_layer = T
    )
    rp$guides("{x: { scale: {type: 'log'}}}")
    rp$guides(
      color = list(
        scale = "#! function(value){
        color_mapping = {dispersion: 'blue', dispGeneEst: 'grey'}
        return color_mapping[value];
  } !#"
      )
    )
    rp$addParams(dom = "DispersionPlot")
  }
  if(input$DEmethod == 'DESeq' | input$DEmethod == 'XBSeq')
  {
    Dispersion$GeneName <- rownames(dataComb[[2]])
    colnames(Dispersion) <- c('PerGeneEst', 'FittedDispEst', 'baseMean', 'GeneName')
    rp <-
      rPlot(
        PerGeneEst ~ baseMean, data = Dispersion, type = 'point', size = list(const = 2),
        tooltip = "#!function(item){ return 'x: ' + item.baseMean + '\\n' +
        ' y: ' + item.PerGeneEst + '\\n' + ' GeneName: ' + item.GeneName}!#"
      )
    rp$guides("{x: { scale: {type: 'log'}}}")
    rp$layer(
      y = 'FittedDispEst', type = 'line', color = list(const = 'red'),
      copy_layer = T
    )
    rp$addParams(dom = "DispersionPlot")
  }
  if(input$DEmethod == 'edgeR'){
    Dispersion$GeneName <- rownames(dataComb[[2]])
    colnames(Dispersion) <- c('CommonDisp', 'TagwiseDisp', 'FittedDisp', 'baseMean', 'GeneName')
    rp <-
      rPlot(
        TagwiseDisp ~ baseMean, data = Dispersion, type = 'point', size = list(const = 2),
        tooltip = "#!function(item){ return 'x: ' + item.baseMean + '\\n' +
        ' y: ' + item.TagwiseDisp + '\\n' + ' GeneName: ' + item.GeneName}!#"
      )
    rp$guides("{x: { scale: {type: 'log'}}}")
    rp$layer(
      y = 'FittedDisp', type = 'line', color = list(const = 'red'),
      copy_layer = T
    )
    rp$layer(
      y = 'CommonDisp', type = 'line', color = list(const = 'blue'),
      copy_layer = T
    )    
    rp$addParams(dom = "DispersionPlot")
  }
  if(input$DEmethod == 'edgeR-robust'){
    Dispersion$GeneName <- rownames(dataComb[[2]])
    colnames(Dispersion) <- c('TagwiseDisp', 'FittedDisp', 'baseMean', 'GeneName')
    rp <-
      rPlot(
        TagwiseDisp ~ baseMean, data = Dispersion, type = 'point', size = list(const = 2),
        tooltip = "#!function(item){ return 'x: ' + item.baseMean + '\\n' +
        ' y: ' + item.TagwiseDisp + '\\n' + ' GeneName: ' + item.GeneName}!#"
      )
    rp$guides("{x: { scale: {type: 'log'}}}")
    rp$layer(
      y = 'FittedDisp', type = 'line', color = list(const = 'red'),
      copy_layer = T
    )
    rp$addParams(dom = "DispersionPlot")
  }
  rp$save('www/report/htmlFiles/DispersionPlot.html', standalone = TRUE)
  rp
})

