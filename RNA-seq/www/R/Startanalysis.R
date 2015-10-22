# output$InputBox <- renderUI(
#   fluidRow(
#     column(width = 12, 
#            box(
#              title = "File input", status = "primary", solidHeader = TRUE,
#              collapsible = TRUE,
#              selectizeInput(
#                "DEmethod", 
#                label = 'Please select a method for DE analysis',
#                choices = c('XBSeq', 'DESeq', 'DESeq2', 'edgeR', 'edgeR-robust', 'limma-voom', 'scde'),
#                options = list(placeholder = 'select a method below',
#                               onInitialize = I('function() { this.setValue(""); }'))
#              ),
#              verbatimTextOutput("value_DE"),
#              fileInput(
#                'file_obs', 'Choose CSV/TXT File for RNA-seq', accept=c('text/csv', 
#                                                                        'text/comma-separated-values,text/plain', 
#                                                                        '.csv')
#              ),
#              conditionalPanel(
#                condition = "input.DEmethod == 'XBSeq'",
#                fileInput(
#                  'file_bg', 'Choose CSV/TXT File for RNA-seq (bg), required if you choose XBSeq', 
#                  accept=c('text/csv',
#                           'text/comma-separated-values,text/plain', 
#                           '.csv')
#                )
#              ),
#              fileInput('file_design', 
#                        'Choose CSV/TXT File for experiment design',
#                        accept=c('text/csv', 
#                                 'text/comma-separated-values,text/plain', 
#                                 '.csv')
#              )
#            ), 
#            box(
#              title = "Options for DE method", status = "info", solidHeader = TRUE,
#              collapsible = TRUE,
#              conditionalPanel(
#                condition = "input.DEmethod == 'DESeq' | input.DEmethod == 'XBSeq'",
#                selectizeInput("SCVmethod", 
#                               label = "Please select a method to estimate dispersion", 
#                               choices =c('pooled', 'per-condition', 'blind'),
#                               selected = 'pooled'
#                ),
#                verbatimTextOutput("SCVmethod"),
#                selectizeInput("SharingMode",
#                               label = "Please select a method for sharing mode",
#                               choices = c('maximum', 'fit-only', 'gene-est-only'),
#                               selected = 'maximum'
#                ),
#                verbatimTextOutput("SharingMode"),
#                selectizeInput("fitType",
#                               label = "Please select a method for fitType",
#                               choices = c('local', 'parametric'),
#                               selected = 'local'
#                ),
#                verbatimTextOutput("fitType"),
#                conditionalPanel(
#                  condition = "input.DEmethod == 'XBSeq'",
#                  selectizeInput("ParamEst", 
#                                 label = "Please select a method to estimate distribution parameters", 
#                                 choices =c('Non-parametric' = 'NP', 
#                                            'Maximum liklihood estimation' = 'MLE'),
#                                 selected = 'NP'
#                  ),
#                  verbatimTextOutput("ParamEst")
#                )
#              ),
#              conditionalPanel(
#                condition = "input.DEmethod == 'DESeq2'",
#                selectizeInput("fitType_DESeq2", 
#                               label = "Please select a method for fit type", 
#                               choices =c('local', 'parametric', 'mean'),
#                               selected = 'local'
#                ),
#                verbatimTextOutput("fitType_DESeq2"),
#                selectizeInput("Test",
#                               label = "Please select a method for statistical test",
#                               choices = c('Wald test' = 'Wald',
#                                           'Log ratio test' = 'LRT'),
#                               selected = 'Wald'
#                ),
#                verbatimTextOutput("Test"),
#                selectizeInput("cooksCutoff",
#                               label = "Please choose either to turn on or off cooks distance cutoff",
#                               choices = c('on',
#                                           'off'),
#                               selected = 'off'
#                ),
#                verbatimTextOutput("cooksCutoff")
#              )
#              #                conditionalPanel(
#              #                  condition = "input.DEmethod == 'edgeR-robust'",
#              #                  selectizeInput("residualType", 
#              #                                 label = "Please select a method for calculating residuals", 
#              #                                 choices =c("pearson", "deviance", "anscombe"),
#              #                                 selected = 'pearson'
#              #                  ),
#              #                  verbatimTextOutput("residualType")
#              #                )
#            )
#     ),
#     column(width = 12, 
#            box(
#              title = "Criteria for DE genes", status = "success", solidHeader = TRUE,
#              collapsible = TRUE,
#              selectizeInput("padjust", 
#                             label = "Please select a method for adjusting p values", 
#                             choices =c("Benj&Hoch" = "BH", 
#                                        "bonferroni", "none"),
#                             selected = 'BH'
#              ),
#              verbatimTextOutput("padjust"),
#              selectizeInput("pcutoff", 
#                             label = "Please set a cutoff of p values for DE genes", 
#                             choices =c(0.001, 0.01, 0.05, 0.1, 0.2),
#                             selected = 0.05
#              ),
#              verbatimTextOutput("pcutoff"),
#              selectizeInput("fccutoff", 
#                             label = "Please set a cutoff of fold change for DE genes", 
#                             choices =c(1.5, 2, 2.5, 3, 5),
#                             selected = 2
#              ),
#              verbatimTextOutput("fccutoff"),
#              numericInput("log2bmcutoff", label = "Please set a cutoff for log2 expression intensity (Usually can be determined from density plot)", 
#                           value = 5, min = 1
#              ),
#              verbatimTextOutput("log2bmcutoff"),
#              actionButton('DEstart', label = 'Start analysis!'),
#              textOutput("DEstart")
#            )
#     )
#   )
# )

output$DEinput <- renderUI({
    if(input$countMatrix == 'Yes' && (input$ExpDesign ==  'Two-level' || (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Specify two conditions to compare')))
      selectizeInput(
        "DEmethod", 
        label = 'Please select a method for DE analysis',
        choices = c('XBSeq', 'DESeq', 'DESeq2', 'edgeR', 'edgeR-robust', 'limma-voom', 'scde', 'SAMSeq'),
        options = list(placeholder = 'select a method below',
                       onInitialize = I('function() { this.setValue(""); }'))
      )
    else if(input$ExpDesign == 'Not available' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Identify most variable genes'))
      selectizeInput(
        "DEmethod", 
        label = 'Please select a method for highly variable genes analysis',
        choices = c('Brennecke_2013'),
        options = list(placeholder = 'select a method below',
                       onInitialize = I('function() { this.setValue(""); }'))
      )
    else if(input$countMatrix == 'No' & (input$ExpDesign ==  'Two-level' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Specify two conditions to compare')))
      selectizeInput(
        "DEmethod", 
        label = 'Please select a method for DE analysis',
        choices = c('monocle', 'limma'),
        options = list(placeholder = 'select a method below',
                       onInitialize = I('function() { this.setValue(""); }'))
      )
})

output$DECriteria <- renderUI({
  if(input$spikein == 'No' & (input$ExpDesign == 'Not available' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Identify most variable genes')))
    {
    tags$div(numericInput("HVGnumber", label = "Please set number of HVGs.", 
                          value = 100, min = 1
    ),
    verbatimTextOutput("HVGnumber_value"))
    }
  else if(1){
    tags$div(
      selectizeInput("padjust", 
                     label = "Please select a method for adjusting p values", 
                     choices =c("Benj&Hoch" = "BH", 
                                "bonferroni", "none"),
                     selected = 'BH'
      ),
      verbatimTextOutput("padjust"),
      selectizeInput("pcutoff", 
                     label = "Please set a cutoff of p values for DE genes/HVGs", 
                     choices =c(0.001, 0.01, 0.05, 0.1, 0.2),
                     selected = 0.05
      ),
      verbatimTextOutput("pcutoff")
    )
  }
  else if((input$ExpDesign == 'Two-level' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Specify two conditions to compare'))){
   tags$div(  selectizeInput("fccutoff", 
                             label = "Please set a cutoff of fold change for DE genes", 
                             choices =c(1.5, 2, 2.5, 3, 5),
                             selected = 2
   ),
   verbatimTextOutput("fccutoff")
   ) 
  }
})

output$Chartpage <- renderUI({
  fluidPage(
    if(input$spikein == 'Yes')
      tabBox(width = 12,
             title = tagList(shiny::icon("tag"), "Quality control"),
             id = "QCtab",
             tabPanel('Spike-ins QC', 
                      fluidRow(column(12, metricsgraphicsOutput('Spikeinsqc')
                      ))),
             tabPanel("Table", 
                      fluidRow(column(12, dataTableOutput("Table")))),
             tabPanel("Heatmap", 
                      fluidRow(column(12, d3heatmapOutput('Heatmap')))), 
             tabPanel("Kernel Density Estimation", 
                      fluidRow(column(12, showOutput("Density", "nvd3")))), 
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
               fluidRow(column(12, showOutput("ScatterPlot", "highcharts")))
             )),
             tabPanel("Boxplot", 
                      fluidRow(column(12, showOutput("Boxplot", "highcharts"))))
      )
    else
      tabBox(width = 12,
           title = tagList(shiny::icon("tag"), "Quality control"),
           id = "QCtab",
           tabPanel("Table", 
                    fluidRow(column(12, dataTableOutput("Table")))),
           tabPanel("Heatmap", 
                    fluidRow(column(12, d3heatmapOutput('Heatmap')))), 
           tabPanel("Kernel Density Estimation", 
                    fluidRow(column(12, showOutput("Density", "nvd3")))), 
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
             fluidRow(column(12, showOutput("ScatterPlot", "highcharts")))
           )),
           tabPanel("Boxplot", 
                    fluidRow(column(12, showOutput("Boxplot", "highcharts"))))
    ),
    tabBox(title = tagList(shiny::icon("tag"), 'Gene/Sample relationship'), id = 'relationTab',
           width = 12,
           tabPanel("Principal Component",
                    fluidRow(
                      column(3, offset = 1, selectizeInput("cvCutoff", 
                                                           label = 'Please select a cutoff for cv (coefficient of variation)',
                                                           choices = c(0.1, 0.3, 0.5))
                      ),
                      column(3,offset = 1, selectizeInput("clusterMethod", 
                                                          label = 'Please select a method for clustering (pca or mds)',
                                                          choices = c('pca', 'mds'))
                      ),
                      column(3, offset = 1, selectizeInput("plotdim", 
                                                           label = 'Please select either 2d or 3d',
                                                           choices = c('2d', '3d')))
                    ),
                    fluidRow(
                      column(3, offset = 1, verbatimTextOutput("value_cvcutoff")),
                      column(3, offset = 1, verbatimTextOutput("value_clustermethod")),
                      column(3, offset = 1, verbatimTextOutput("value_plotdim"))
                    ),
                    hr(),
                    #                if(input$value_plotdim == '2d')
                    #                  fluidRow(column(12, showOutput("PrincipalComponent2d", "dimple")))
                    #                else
                    #                  fluidRow(column(12, scatterplotThreeOutput("PrincipalComponent3d")))
                    fluidRow(
                      conditionalPanel(condition = "input.plotdim =='2d'",
                                       column(12, showOutput("PrincipalComponent2d", "dimple"))),
                      conditionalPanel(condition = "input.plotdim =='3d'",
                                       column(12, scatterplotThreeOutput("PrincipalComponent3d")))
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
             fluidRow(
               column(12, forceNetworkOutput("forceNetworkGene"))
             )
           )
    ),
    if(input$DEmethod != 'limma-voom' & input$DEmethod != 'scde' & input$DEmethod != 'limma' & input$DEmethod != 'monocle' & input$DEmethod != 'SAMSeq' & input$DEmethod != 'Brennecke_2013')
      tabBox(
        title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
        id = 'DEanalysis', width = 12, 
        tabPanel("DE Table", 
                 fluidRow(column(12, dataTableOutput('DEtable')))),
        tabPanel("MAplot", 
                 fluidRow(column(12, metricsgraphicsOutput("MAplot")))),
        tabPanel("DE Heatmap", 
                 fluidRow(column(12, d3heatmapOutput('DEheatmap')))),
        tabPanel("Dispersion plot", 
                 fluidRow(column(12, showOutput("DispersionPlot", "polycharts"))))
      )
    else if(input$DEmethod != 'Brennecke_2013')
      tabBox(
        title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
        id = 'DEanalysis', width = 12, 
        tabPanel("DE Table", 
                 fluidRow(column(12, dataTableOutput('DEtable')))),
        tabPanel("MAplot", 
                 fluidRow(column(12, metricsgraphicsOutput("MAplot")))),
        tabPanel("DE Heatmap", 
                 fluidRow(column(12, d3heatmapOutput('DEheatmap'))))
      )
    else
      tabBox(
        title = tagList(shiny::icon("tag"), 'Highly variable genes analysis'),
        id = 'DEanalysis', width = 12, 
        tabPanel("HVGs Table", 
                 fluidRow(column(12, dataTableOutput('HVGtable')))),
        tabPanel("HVGs Heatmap", 
                 fluidRow(column(12, d3heatmapOutput('HVGheatmap'))))
      ),
    box(title = 'File exports', collapsible = T, status = 'success', width = 12,
        uiOutput('DownloadUI')
    )
  )
})

outputfiles <- reactive({
  dataComb <- dataComb()
  folder <- dataComb[[5]]
  path <- paste(folder, '/htmlFiles', sep='')
  nfiles <- length(dir(path))
  return(nfiles)
})

output$DownloadUI <- renderUI({
  nfiles <- outputfiles()
    if(nfiles >= 5)
      downloadButton('StartDownload', label = "Download")
  })

output$StartDownload <- downloadHandler(
  filename <- 'Report.zip',
  content <- function(file) {
    path.old <- getwd()
    dataComb <- dataComb()
    path <- dataComb[[5]]
    setwd(path)
    on.exit({
      setwd(path.old)
      unlink(path, recursive = TRUE, force = TRUE)
    })
    if(input$DEmethod != 'limma-voom' & input$DEmethod != 'scde'){
      slidify('Report.Rmd')
      zip(file, files = c('Report.html', 'libraries/', 'htmlFiles/', 'DEstat.csv', 'TestStat.csv'))
    }
    else{
      slidify('Reports.Rmd')
      zip(file, files = c('Reports.html', 'libraries/', 'htmlFiles/', 'DEstat.csv', 'TestStat.csv'))
    }
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
  if(input$ExpDesign == 'Not available')
    return(NULL)
  else{
    inFile <- input$file_design
    if (is.null(inFile))
      return(NULL)
    design <- fread(inFile$datapath, data.table = F)
    design[[1]]
  }
})

dataComb <- eventReactive(input$DEstart, {
  inFile <- input$file_obs
  if (is.null(inFile))
    return(NULL)
  withProgress(value = 1, message = 'Loading datasets', {
    data_obs <- fread(inFile$datapath, data.table = F)
    rownames(data_obs) <- data_obs[,1]
    data_obs <- data_obs[,-1]
    if(!is.null(input$file_spikein)){
      data_spikein <- fread(input$file_spikein$datapath, data.table = F)
      rownames(data_spikein) <- data_spikein[,1]
      data_spikein <- data_spikein[,-1]
      Totalreads <- apply(data_obs, 2, sum) + apply(data_spikein, 2, sum)
      data_qc <- data.frame(
        Totalreads = Totalreads,
        Spikein_prop = apply(data_spikein, 2, sum)*100/Totalreads
      )
      colnames(data_qc) <- c('Totalreads', 'Spikein_prop')
    }
    else{
      data_qc <- c()
      data_spikein <- c()
    }
    if(input$ExpDesign == 'Not available')
      group <- rep('C1', ncol(data_obs))
    else
      group <- design()
    for(i in 1:100){
      folder <- paste('www/report/user/user', i, sep='')
      if(!dir.exists(folder)){
        dir.create(folder)
        dir.create(paste(folder, '/htmlFiles', sep = ''))
        file.copy(c('www/report/Report.Rmd', 'www/report/Reports.Rmd', 'www/report/libraries'), folder, recursive = TRUE)
        break
      }
    }
  })
  withProgress(value = 1, message = paste('Statistical testing using ', input$DEmethod, sep =''), {
    if (input$DEmethod == 'XBSeq') {
      data_bg <- fread(input$file_bg$datapath, data.table = F)
      rownames(data_bg) <- data_bg[,1]
      data_bg <- data_bg[,-1]
      dataOut <- XBSeq_pfun(data_obs, data_bg, group, disp_method = input$SCVmethod, 
                            sharing_mode = input$SharingMode, fit_type = input$fitType,
                            paraMethod = input$ParamEst, spikeins = data_spikein, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'DESeq') {
      dataOut <- DESeq_pfun(data_obs, group, disp_method = input$SCVmethod,
                            sharing_mode = input$SharingMode, 
                            fit_type = input$fitType, spikeins = data_spikein, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'DESeq2') {
      dataOut <- DESeq2_pfun(
        data_obs, group, cookcutoff = input$cooksCutoff,
        fittype = input$fitType_DESeq2, test = input$Test, spikein = data_spikein, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'edgeR') {
      dataOut <- edgeR.pfun(data_obs, group, model.matrix(~group), spikeins = data_spikein, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'edgeR-robust') {
      dataOut <- edgeR_robust.pfun(data_obs, group, model.matrix(~group), spikeins = data_spikein, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'limma-voom') {
      dataOut <- limma_voom.pfun(data_obs, group, model.matrix(~group), spikeins = data_spikein, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'scde') {
      dataOut <- scde.pfun(data_obs, group, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'limma'){
      dataOut <- limma.pfun(data_obs, group, model.matrix(~0 + group), condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'monocle'){
      dataOut <- monocle.pfun(data_obs, group, condition_sel = c(input$Con_S1, input$Con_S2))
    }
    else if (input$DEmethod == 'Brennecke_2013'){
      Brennecke_pfun(data_obs, spikeins = data_spikein)
    }
    else if (input$DEmethod == 'SAMSeq'){
      SAMSeq.pfun(data_obs, group, condition_sel = c(input$Con_S1, input$Con_S2))
    }
  })
  dataOut[[5]] <- folder
  dataOut[[6]] <- data_qc
  return(dataOut)
})

output$value_DE <- renderPrint({
  input$DEmethod
})

output$value_countMatrix <- renderPrint({
  input$countMatrix
})

output$value_ExpDesign <- renderPrint({
  input$ExpDesign
})

output$value_MultiLevel <- renderPrint({
  input$MultiLevel
})

output$value_spikein <- renderPrint({
  input$spikein
})

output$value_Con_S1 <- renderPrint({
  input$Con_S1
})

output$value_Con_S2 <- renderPrint({
  input$Con_S2
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

output$log2bmcutoff_value <- renderPrint({
  input$log2bmcutoff
})

output$HVGnumber_value <- renderPrint({
  input$HVGnumber
})

output$Spikeinsqc <- renderMetricsgraphics({
  if (is.null(input$file_spikein))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Spike-ins QC', {
    folder <- dataComb[[5]]
    dataMat <- dataComb[[6]]
    rownames(dataMat) <- paste('S', 1:nrow(dataMat), sep = '')
    mp <-
      mjs_plot(dataMat, Totalreads, Spikein_prop, decimals = 6) %>%
      mjs_point(
        x_rug = TRUE, y_rug = TRUE
      ) %>%
      #    mjs_add_baseline(y_value = 0, label = 'baseline') %>%
      mjs_labs(x_label = 'Number of mapped reads', y_label = "Spike-in reads (%)")
    saveWidget(mp, file = 'SpikeinQC.html')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./SpikeinQC.html', path)
    file.remove('./SpikeinQC.html')
    mp
      })
})

output$Table <- DT::renderDataTable({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Table', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
    temp <- datatable(dataMat, options = list(pageLength = 5))
    saveWidget(temp, 'datatable.html')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./datatable.html', path)
    file.remove('./datatable.html')
    temp
  })
})

output$Heatmap <- renderD3heatmap({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Heatmap', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
    mean_gene <- apply(dataMat, 1, mean)
    var_gene <- apply(dataMat, 1, var)
    index <- which(mean_gene > as.numeric(input$log2bmcutoff))
    dataMat1 <- dataMat[order(var_gene[index]),]
    if (!length(index))
      return(NULL)
    uplim <- ifelse(length(index) < 1000, length(index), 1000)
    h1 <- d3heatmap(as.data.frame(dataMat1[1:uplim,]), scale = "row", colors = colorRampPalette(c("blue","white","red"))(1000), Colv = FALSE)
    saveWidget(h1, 'heatmap.html')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./heatmap.html', path)
    file.remove('./heatmap.html')
    h1
    })

})

output$Density <- renderChart({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Density', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
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
    path <- paste(folder, '/htmlFiles/density.html', sep = '')
    np$save(path, standalone = TRUE)
    return(np)  
    })

})

output$value_S1 <- renderPrint({
  input$text_S1
})

output$value_S2 <- renderPrint({
  input$text_S2
})

output$ScatterPlot <- renderChart({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Scatterplot', {
    folder <- dataComb[[5]]
    dataMat <- log2(dataComb[[2]] + 0.25)
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
    path <- paste(folder, '/htmlFiles/Scatterplot.html', sep = '')
    hp$save(path, standalone = TRUE)
    hp
    })
})

output$value_cvcutoff <- renderPrint({
  input$cvCutoff
})

output$value_clustermethod <-
  renderPrint({
    input$clusterMethod
  })

output$value_plotdim <- 
  renderPrint({
    input$plotdim
  })

output$Boxplot <- renderChart({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Boxplot', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
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
    path <- paste(folder, '/htmlFiles/Boxplot.html', sep = '')
    hp$save(path, standalone = TRUE)
    hp
    })
  })

Principalstats <- reactive({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Calculating PCA stats', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
    if(input$ExpDesign == 'Not available')
      design <- rep('C1', ncol(dataMat))
    else
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
      mds.result <- cmdscale(dd, k = 3, eig = TRUE)
      ppoints <- mds.result$points
      eig <- mds.result$eig
      percent <- round(eig/sum(eig) * 100, 1)
    }
    else{
      pca.result <- prcomp(t(dataMat))
      ppoints <- pca.result$x[,1:3]
      percent<-round((pca.result$sdev^2/sum(pca.result$sdev^2))*100,1)
    }
    ppoints <-
      cbind(ppoints, design, paste('S', 1:ncol(dataMat), sep = ''))
    colnames(ppoints) <- c('PC1', 'PC2', 'PC3', 'Design', 'Samplename')
    ppoints <- as.data.frame(ppoints)
    return(list(ppoints, percent, folder))
  })
})

output$PrincipalComponent2d <- 
  renderChart({
    prinstat <- Principalstats()
    withProgress(value = 1, message = 'Generating plots: ', detail = 'PCA plot', {
      ppoints <- prinstat[[1]]
      percent <- prinstat[[2]]
      folder <- prinstat[[3]]
      xlab <- paste('PC1 (', percent[1],'%)', sep = '')
      ylab <- paste('PC2 (', percent[2], '%)', sep = '')
      xlab <- paste('PC1 (', percent[1],'%)', sep = '')
      ylab <- paste('PC2 (', percent[2], '%)', sep = '')
      dp <-
        dPlot(
          PC2 ~ PC1, data = as.data.frame(ppoints), 
          groups = c('Samplename', 'Design'), type = 'bubble',
          xlab = xlab, ylab = ylab)
      dp$xAxis(type = 'addMeasureAxis')
      dp$addParams(dom = "PrincipalComponent2d")
      path <- paste(folder, '/htmlFiles/pcaplot.html', sep = '')
      dp$save(path, cdn = TRUE)
      dp
      })
  })

output$PrincipalComponent3d <- renderScatterplotThree({
  prinstat <- Principalstats()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'PCA plot', {
    ppoints <- prinstat[[1]]
    percent <- prinstat[[2]]
    folder <- prinstat[[3]]
    xlab <- paste('PC1 (', percent[1],'%)', sep = '')
    ylab <- paste('PC2 (', percent[2], '%)', sep = '')
    zlab <- paste('PC3 (', percent[3], '%)', sep = '')
    colors <-
      c(
        '#00FFFF', '#FFE4C4', '#D2691E', '#6495ED', '#9932CC', '#8B0000',
        '#FF00FF', '#FFD700'
      )
    design <- design()
    col <- colors[1:length(unique(design))]
    col <- rep(col, c(as.numeric(table(design))))
    scatter3d <- scatterplot3js(ppoints[,1], ppoints[,2], ppoints[,3], 
                                labels = ppoints$Samplename, 
                                axisLabels = c(xlab, ylab, zlab),
                                color = col,
                                renderer = 'canvas')
    saveWidget(scatter3d, 'pcaplot.html')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./pcaplot.html', path)
    file.remove('./pcaplot.html')
    return(scatter3d)
    })
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
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Gene interaction network', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
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
      path <- paste(folder, '/htmlFiles/', sep = '')
      file.copy('./forceNet.html', path)
      file.remove('./forceNet.html')
      return(forceNet)
    }
    })

})

output$DEtable <- DT::renderDataTable({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'DE table', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
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
      dataMat2 <- c()
    else{
      dataMat2 <-
        cbind(dataMat[DE_index,], dataMat1[DE_index,2], p_adjust1[DE_index])
      colnames(dataMat2) <-
        c(paste('S', 1:ncol(dataMat), sep = ''), 'Log2 fold change', 'p adjusted value')
    }
    temp <- datatable(dataMat2, options = list(pageLength = 5))
    saveWidget(temp, 'DEdatatable.html')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./DEdatatable.html', path)
    file.remove('./DEdatatable.html')
    temp
      })
})

output$DEheatmap <- renderD3heatmap({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'DE heatmap', {
    dataMat <- log2(dataComb[[2]] + 0.25)
    folder <- dataComb[[5]]
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
    if (length(DE_index) <= 2){
      htmltools::save_html(c(), 'DEheatmap.html')
      path <- paste(folder, '/htmlFiles/', sep = '')
      file.copy('./DEheatmap.html', path)
      file.remove('./DEheatmap.html')
      return(NULL)
    }
    dataMat2 <-dataMat[DE_index,]
    colnames(dataMat2) <- paste('S', 1:ncol(dataMat), sep = '')
    h1 <- d3heatmap(dataMat2, scale = "row", colors = colorRampPalette(c("blue","white","red"))(1000), Colv = FALSE)
    saveWidget(h1, 'DEheatmap.html')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./DEheatmap.html', path)
    file.remove('./DEheatmap.html')
    h1
  })
})

output$MAplot <- renderMetricsgraphics({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'MAplot', {
    dataMat <- dataComb[[4]]
    folder <- dataComb[[5]]
    colnames(dataMat) <- c('baseMean', 'log2FoldChange', 'p_adjust')
    p_adjust1 <- p.adjust(dataMat[,3], method = input$padjust)
    p_adjust1[is.na(p_adjust1)] <- 1
    col <-
      with(
        data = dataMat, ifelse(
          log2(baseMean + 0.25) > as.numeric(input$log2bmcutoff) &
            abs(log2FoldChange) > log2(as.numeric(input$fccutoff)) &
            p_adjust1 < as.numeric(input$pcutoff),
          "DE", "Not DE"
        )
      )
    dataout <- cbind(dataComb[[2]], dataMat[,2], p_adjust1)
    colnames(dataout) <- c(colnames(dataComb[[2]]), 'Log2 Fold change', 'p value')
    path <- paste(folder, sep = '')
    write.csv(dataout, paste(path, '/TestStat.csv', sep =''), quote = F)
    write.csv(dataout[col=='DE',], paste(path, '/DEstat.csv', sep =''), quote = F)
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
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./MAplot.html', path)
    file.remove('./MAplot.html')
    mp
    })
})

output$DispersionPlot <- renderChart3({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Dispersion plot', {
    Dispersion <- dataComb[[3]]
    folder <- dataComb[[5]]
    Dispersion$baseMean <- dataComb[[4]][,1] + 0.25
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
    path <- paste(folder, '/htmlFiles/DispersionPlot.html', sep = '')
    rp$save(path, standalone = TRUE)
    rp
    })
})

# output$progressbar <- renderUI({
#     input$DEstart
#     progress <- shiny::Progress$new()
#     on.exit(progress$close())
#     progress$set(message = "Differential expression testing", value = 0)
#     n <- 10^6
#     for (i in 1:n) {
#       dataComb <- dataComb()
#       folder <- dataComb[[5]]
#       path <- paste(folder, '/htmlFiles', sep='')
#       nfiles <- length(dir(path))
#       progress$set(value = nfiles/10, message = 'Generating figures',detail = paste(100*(round(nfiles/10, 2)), '% completed', sep =''))
#       if(nfiles >= 10)
#         break
#     }
#   })



# output$XBplot <- renderChart({
#   
# })


