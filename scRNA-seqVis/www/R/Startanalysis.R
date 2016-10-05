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
    if(input$countMatrix == 'Yes' && (input$ExpDesign ==  'Two-level' || (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Specify two conditions to compare'))){
      tags$div(selectizeInput(
        "DEmethod", 
        label = 'Please select a method for DE analysis',
        choices = c('XBSeq', 'DESeq', 'DESeq2', 'edgeR', 'edgeR-robust', 'limma-voom', 'scde', 'BPSC', 'EBSeq', 'ROTS'),
        options = list(placeholder = 'select a method below',
                       onInitialize = I('function() { this.setValue(""); }'))
      ),
      verbatimTextOutput("DEmethod_value"))
    }
  else if(input$countMatrix == 'No' & (input$ExpDesign ==  'Two-level' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Specify two conditions to compare'))){
    tags$div(selectizeInput(
      "DEmethod", 
      label = 'Please select a method for DE analysis',
      choices = c('monocle', 'limma', 'edgeR', 'edgeR-robust', 'BPSC', 'MAST', 'EBSeq', 'ROTS'),
      options = list(placeholder = 'select a method below',
                     onInitialize = I('function() { this.setValue(""); }'))
    ),
    verbatimTextOutput("DEmethod_value"))
  }
  else if((input$ExpDesign == 'Not available' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Identify most variable genes')) & input$countMatrix == 'Yes' ) 
        tags$div(
          selectizeInput(
            "DEmethod", 
            label = 'Please select a method for highly variable genes analysis',
            choices = c('Brennecke_2013'),
            options = list(placeholder = 'select a method below',
                           onInitialize = I('function() { this.setValue(""); }'))
          ),
          verbatimTextOutput("DEmethod_value"),
          selectizeInput(
          "DEmethod1", 
          label = 'Please select a method to characterize subpopulation',
          choices = c('XBSeq', 'DESeq', 'DESeq2', 'edgeR', 'edgeR-robust', 'limma-voom', 'scde', 'BPSC', 'EBSeq', 'ROTS'),
          options = list(placeholder = 'select a method below',
                         onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("DEmethod_value1")
        )
  else if((input$ExpDesign == 'Not available' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Identify most variable genes')) & input$countMatrix == 'No')
        tags$div(
          selectizeInput(
            "DEmethod", 
            label = 'Please select a method for highly variable genes analysis',
            choices = c('Brennecke_2013'),
            options = list(placeholder = 'select a method below',
                           onInitialize = I('function() { this.setValue(""); }'))
          ),
          verbatimTextOutput("DEmethod_value"),
          selectizeInput(
          "DEmethod1", 
          label = 'Please select a method to characterize subpopulation',
          choices = c('monocle', 'limma', 'edgeR', 'edgeR-robust', 'BPSC', 'MAST', 'EBSeq', 'ROTS'),
          options = list(placeholder = 'select a method below',
                         onInitialize = I('function() { this.setValue(""); }'))
        ),
        verbatimTextOutput("DEmethod_value1")
        )
})

output$HVGBox <- renderUI({
  if(input$ExpDesign == 'Not available' || (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Identify most variable genes'))
  {
    box(
      title = "Criteria for HVG genes", status = 'danger', solidHeader = TRUE,width = NULL,
      collapsible = TRUE,
      if(input$spikein == 'No')
      {
        tags$div(numericInput("HVGnumber", label = "Please set number of HVGs.", 
                              value = 100, min = 1
        ),
        verbatimTextOutput("HVGnumber_value")
    )
      }
  else{
    tags$div(
      selectizeInput("HVGpadjust", 
                     label = "Please select a method for adjusting p values", 
                     choices =c("Benj&Hoch" = "BH", 
                                "bonferroni", "none"),
                     selected = 'BH'
      ),
      verbatimTextOutput("HVGpadjust_value"),
      selectizeInput("HVGpcutoff", 
                     label = "Please set a cutoff of p values for DE genes/HVGs", 
                     choices =c(0.001, 0.01, 0.05, 0.1, 0.2),
                     selected = 0.05
      ),
      verbatimTextOutput("HVGpcutoff_value")
    )
  }
  )}
  })

output$Chartpage <- renderUI({
  fluidPage(
    if(input$spikein == 'Yes')
      tabBox(width = 12,
             title = tagList(shiny::icon("tag"), "Quality control"),
             id = "QCtab",
             tabPanel('Spike-ins QC', 
                      fluidRow(column(12, metricsgraphics::metricsgraphicsOutput('Spikeinsqc')
                      ))),
             tabPanel("Table", 
                      fluidRow(column(12, DT::dataTableOutput("Table")))),
             tabPanel("Heatmap", 
                      fluidRow(column(12, d3heatmap::d3heatmapOutput('Heatmap')))), 
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
                    fluidRow(column(12, DT::dataTableOutput("Table")))),
           tabPanel("Heatmap", 
                    fluidRow(column(12, d3heatmap::d3heatmapOutput('Heatmap')))), 
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
                                       column(12, threejs::scatterplotThreeOutput("PrincipalComponent3d")))
                    )
           ),
           tabPanel("Gene interaction network", fluidPage(
             fluidRow(
               column(4, offset = 1, selectizeInput("Exprscut", 
                                                    label = 'Expression level cutoff in quantile',
                                                    choices = c('0% quantile', '25% quantile', '50% quantile', '75% quantile', '100% quantile'),
                                                    selected = "75% quantile"
                                                    )),
               column(4,offset = 2, sliderInput("Corrcut", "Correlation cutoff", 
                                                min=0, max=1, step = 0.05, value=0.95)
               )),
             fluidRow(
               column(4, offset = 1, verbatimTextOutput("value_Exprscut")),
               column(4, offset = 2, verbatimTextOutput("value_Corrcut"))
             )),
             hr(),
             fluidRow(
               column(12, networkD3::forceNetworkOutput("forceNetworkGene"))
             )
           )
    ),
    if(input$DEmethod == 'XBSeq' | input$DEmethod == 'DESeq' | input$DEmethod == 'DESeq2' | input$DEmethod == 'edgeR' | input$DEmethod == 'edgeR-robust')
      tabBox(
        title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
        id = 'DEanalysis', width = 12, 
        tabPanel("DE Table", 
                 fluidRow(column(12, DT::dataTableOutput('DEtable')))),
        tabPanel("MAplot", 
                 fluidRow(column(12, metricsgraphics::metricsgraphicsOutput("MAplot")))),
        tabPanel("DE Heatmap", 
                 fluidRow(column(12, d3heatmap::d3heatmapOutput('DEheatmap')))),
        tabPanel("Dispersion plot", 
                 fluidRow(column(12, showOutput("DispersionPlot", "polycharts"))))
      )
    else if(input$DEmethod == 'limma-voom' | input$DEmethod == 'scde' | input$DEmethod == 'limma' | input$DEmethod == 'monocle' | input$DEmethod == 'BPSC' | input$DEmethod == 'MAST' | input$DEmethod == 'EBSeq' | input$DEmethod == 'ROTS')
            tabBox(
              title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
              id = 'DEanalysis', width = 12, 
              tabPanel("DE Table", 
                       fluidRow(column(12, DT::dataTableOutput('DEtable')))),
              tabPanel("MAplot", 
                       fluidRow(column(12, metricsgraphics::metricsgraphicsOutput("MAplot")))),
              tabPanel("DE Heatmap", 
                       fluidRow(column(12, d3heatmap::d3heatmapOutput('DEheatmap'))))
            )
      else
        tabBox(
          title = tagList(shiny::icon("tag"), 'Highly variable genes analysis'),
          id = 'DEanalysis', width = 12, 
          tabPanel("HVGs Table", 
                   fluidRow(column(12, DT::dataTableOutput('HVGtable')))),
          tabPanel("HVGs Heatmap", 
                   fluidRow(column(12, d3heatmap::d3heatmapOutput('HVGheatmap')))),
          tabPanel("HVG plot", 
                   fluidRow(column(12, metricsgraphics::metricsgraphicsOutput('HVGplot'))))
        ),  
#     if(input$DEmethod != 'limma-voom' & input$DEmethod != 'scde' & input$DEmethod != 'limma' & input$DEmethod != 'monocle' & input$DEmethod != 'Brennecke_2013')
#       tabBox(
#         title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
#         id = 'DEanalysis', width = 12, 
#         tabPanel("DE Table", 
#                  fluidRow(column(12, dataTableOutput('DEtable')))),
#         tabPanel("MAplot", 
#                  fluidRow(column(12, metricsgraphicsOutput("MAplot")))),
#         tabPanel("DE Heatmap", 
#                  fluidRow(column(12, d3heatmapOutput('DEheatmap')))),
#         tabPanel("Dispersion plot", 
#                  fluidRow(column(12, showOutput("DispersionPlot", "polycharts"))))
#       )
#     else if(input$DEmethod != 'Brennecke_2013')
#       tabBox(
#         title = tagList(shiny::icon("tag"), 'Differential expression analysis'),
#         id = 'DEanalysis', width = 12, 
#         tabPanel("DE Table", 
#                  fluidRow(column(12, dataTableOutput('DEtable')))),
#         tabPanel("MAplot", 
#                  fluidRow(column(12, metricsgraphicsOutput("MAplot")))),
#         tabPanel("DE Heatmap", 
#                  fluidRow(column(12, d3heatmapOutput('DEheatmap'))))
#       )
#     else
#       tabBox(
#         title = tagList(shiny::icon("tag"), 'Highly variable genes analysis'),
#         id = 'DEanalysis', width = 12, 
#         tabPanel("HVGs Table", 
#                  fluidRow(column(12, dataTableOutput('HVGtable')))),
#         tabPanel("HVGs Heatmap", 
#                  fluidRow(column(12, d3heatmapOutput('HVGheatmap')))),
#         tabPanel("HVG plot", 
#                  fluidRow(column(12, d3heatmapOutput('HVGplot'))))
#       ),
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
    if(input$DEmethod == 'XBSeq' | input$DEmethod == 'DESeq' | input$DEmethod == 'DESeq2' | input$DEmethod == 'edgeR' | input$DEmethod == 'edgeR-robust'){
      slidify::slidify('Report.Rmd')
      zip(file, files = c('Report.html', 'libraries/', 'htmlFiles/', 'DEstat.csv', 'TestStat.csv', 'pdfFiles'))
    }
    else if (input$DEmethod == 'Brennecke_2013'){
      slidify::slidify('ReportRNASeqVis.Rmd')
      if(dir.exists('HeteroModule/'))
        zip(file, files = c('ReportRNASeqVis.html', 'libraries/', 'htmlFiles/', 'NormData.csv', 'HVGData.csv', 'HeteroModule/'))
      else
        zip(file, files = c('ReportRNASeqVis.html', 'libraries/', 'htmlFiles/', 'NormData.csv', 'HVGData.csv', 'pdfFiles'))
    }
    else{
      slidify::slidify('Reports.Rmd')
      zip(file, files = c('Reports.html', 'libraries/', 'htmlFiles/', 'DEstat.csv', 'TestStat.csv', 'pdfFiles'))
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
    design <- data.table::fread(inFile$datapath, data.table = F)
    design[[1]]
  }
})

dataComb <- eventReactive(input$DEstart, {
  inFile <- input$file_obs
  if (is.null(inFile))
    return(NULL)
  withProgress(value = 1, message = 'Loading datasets', {
    data_obs <- data.table::fread(inFile$datapath, data.table = F)
    cat('checkpoint1', '\n')
    rownames(data_obs) <- data_obs[,1]
    data_obs <- data_obs[,-1]
    if(!is.null(input$file_spikein)){
      data_spikein <- data.table::fread(input$file_spikein$datapath, data.table = F)
      cat('checkpoint2', '\n')
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
        dir.create(paste(folder, '/pdfFiles', sep = ''))
        file.copy(c('www/report/Report.Rmd', 'www/report/Reports.Rmd', 'www/report/ReportRNASeqVis.Rmd', 'www/report/libraries'), folder, recursive = TRUE)
        break
      }
    }
  })
  withProgress(value = 1, message = paste('Statistical testing using ', input$DEmethod, sep =''), {
    if(input$Con_S1 == '' & input$Con_S2 == '')
      condition_sel <- c()
    else
      condition_sel <- c(input$Con_S1, input$Con_S2)
    if (input$DEmethod == 'XBSeq') {
      data_bg <- data.table::fread(input$file_bg$datapath, data.table = F)
      rownames(data_bg) <- data_bg[,1]
      data_bg <- data_bg[,-1]
      dataOut <- XBSeq.pfun(data_obs, data_bg, group, disp_method = input$SCVmethod, 
                            sharing_mode = input$SharingMode, fit_type = input$fitType,
                            paraMethod = input$ParamEst, spikeins = data_spikein, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'DESeq') {
      dataOut <- DESeq.pfun(data_obs, group, disp_method = input$SCVmethod,
                            sharing_mode = input$SharingMode, 
                            fit_type = input$fitType, spikeins = data_spikein, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'DESeq2') {
      dataOut <- DESeq2.pfun(
        data_obs, group, cookcutoff = input$cooksCutoff,
        fittype = input$fitType_DESeq2, test = input$Test, spikein = data_spikein, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'edgeR') {
      dataOut <- edgeR.pfun(data_obs, group, model.matrix(~group), spikeins = data_spikein, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'edgeR-robust') {
      dataOut <- edgeR_robust.pfun(data_obs, group, model.matrix(~group), spikeins = data_spikein, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'limma-voom') {
      dataOut <- limma_voom.pfun(data_obs, group, model.matrix(~group), spikeins = data_spikein, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'scde') {
      dataOut <- scde.pfun(data_obs, group, condition_sel = condition_sel)
      }
    else if (input$DEmethod == 'limma'){
      dataOut <- limma.pfun(data_obs, group, model.matrix(~group), condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'monocle'){
      dataOut <- monocle.pfun(data_obs, group, condition_sel = condition_sel)
    }
    else if (input$DEmethod == 'Brennecke_2013'){
      dataOut <- Brennecke.pfun(data_obs, spikeins = data_spikein, input$HVGnumber)
    }
    else if (input$DEmethod == 'BPSC')
      dataOut <- BPSC.pfun(data_obs, group, model.matrix(~group), data_spikein, condition_sel, 4)
    else if (input$DEmethod == 'MAST'){
      cat('checkpoint3', '\n')
      dataOut <- MAST.pfun(data_obs, group, data_spikein, condition_sel)
    }
    else if (input$DEmethod == 'EBSeq')
      dataOut <- EBSeq.pfun(data_obs, group, condition_sel)
    else if (input$DEmethod == 'ROTS')
      dataOut <- ROTS.pfun(data_obs, group, condition_sel)
    
#     else if (input$DEmethod == 'SAMSeq'){
#       SAMSeq.pfun(data_obs, group, condition_sel = c(input$Con_S1, input$Con_S2))
#     }
  })
  dataOut[[5]] <- folder
  dataOut[[6]] <- data_qc
  dataOut[[7]] <- data_spikein
  cat('Statistical testing success!', '\n')
  return(dataOut)
})

output$value_countMatrix <- renderPrint({
  input$countMatrix
})

output$value_countMatrix1 <- renderPrint({
  input$countMatrix
})

output$value_ExpDesign <- renderPrint({
  input$ExpDesign
})

output$value_MultiLevel <- renderPrint({
  input$MultiLevel
})

output$value_deterHetero <- renderPrint({
  input$deterHetero
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

output$DEmethod_value <- renderPrint({
  input$DEmethod
})

output$DEmethod_value1 <- renderPrint({
  input$DEmethod1
})

output$HVGpadjust_value <- renderPrint({
  input$HVGpadjust
})

output$HVGpcutoff_value <- renderPrint({
  input$HVGpcutoff
})

output$Spikeinsqc <- metricsgraphics::renderMetricsgraphics({
  if (is.null(input$file_spikein))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Spike-ins QC', {
    folder <- dataComb[[5]]
    dataMat <- dataComb[[6]]
    rownames(dataMat) <- paste('S', 1:nrow(dataMat), sep = '')
    mp <-
      metricsgraphics::mjs_plot(dataMat, Totalreads, Spikein_prop, decimals = 6) %>%
      metricsgraphics::mjs_point(
        x_rug = TRUE, y_rug = TRUE
      ) %>%
      #    mjs_add_baseline(y_value = 0, label = 'baseline') %>%
      metricsgraphics::mjs_labs(x_label = 'Number of mapped reads', y_label = "Spike-in reads (%)")
    gp <- ggplot() + geom_point(aes(x = Totalreads, y = Spikein_prop), data = dataMat) + labs(x = 'Number of mapped reads', y = "Spike-in reads (%)")
    ggsave(paste(folder, '/pdfFiles/', 'spikeinQC.pdf', sep = ''), gp)
    htmlwidgets::saveWidget(mp, file = 'SpikeinQC.html', background = 'none')
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
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
    temp <- DT::datatable(dataMat, options = list(pageLength = 5))
    htmlwidgets::saveWidget(temp, 'datatable.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./datatable.html', path)
    file.remove('./datatable.html')
    temp
  })
})

output$Heatmap <- d3heatmap::renderD3heatmap({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Heatmap', {
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    mean_gene <- apply(dataMat, 1, mean)
    var_gene <- apply(dataMat, 1, var)
    meanCutQ <- switch(input$log2bmcutoff,
                      "0% quantile" = 1,
                      "25% quantile" = 2,
                      "50% quantile" = 3,
                      "75% quantile" = 4,
                      "100% quantile" =5)
    index <- which(mean_gene > quantile(mean_gene,na.rm = TRUE)[meanCutQ])
    dataMat1 <- dataMat[order(var_gene[index]),]
    if (!length(index))
      return(NULL)
    uplim <- ifelse(length(index) < 1000, length(index), 1000)
    h1 <- d3heatmap::d3heatmap(as.data.frame(dataMat1[1:uplim,]), scale = "row", colors = colorRampPalette(c("blue","white","red"))(1000), Colv = FALSE, show_grid = F)
    htmlwidgets::saveWidget(h1, 'heatmap.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./heatmap.html', path)
    file.remove('./heatmap.html')
    pdf(paste(folder, '/pdfFiles/', 'heatmap.pdf', sep = ''))
    heatmap.my(as.matrix(dataMat1[1:uplim,]))
    dev.off()
    h1
    })
})

output$Density <- renderChart({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Density', {
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    denStat <-
      density(dataMat[,1], from = min(dataMat, na.rm = TRUE), to = max(dataMat, na.rm = TRUE), na.rm=TRUE)
    denStat <- data.frame(x = denStat$x,
                          y = denStat$y)
    denStat1 <- sapply(1:ncol(dataMat), function(i) {
      preset <-
        density(dataMat[,i], from = min(dataMat, na.rm = TRUE), to = max(dataMat, na.rm = TRUE), na.rm = TRUE)
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
    if(input$DEmethod == 'edgeR' | input$DEmethod == 'edgeR-robust')
      xlab <- 'Log2 normalized cpm (counts per million)'
    else
      xlab <- 'Log2 normalized intensity'
    np$xAxis(axisLabel = xlab)
    np$yAxis(axisLabel = 'Density')
    path <- paste(folder, '/htmlFiles/density.html', sep = '')
    dp <- ggplot() + geom_line(aes(x = Exprs, y = Density, color = ind), data = denStat) + 
      guides(color = 'none') + labs(x = xlab, y = 'density')
    ggsave(paste(folder, '/pdfFiles/density.pdf', sep = ''), dp)
    if(ncol(dataMat) > 100)
      return(NULL)
      #np$chart(useInteractiveGuideline = FALSE, showLegend = FALSE, interactive = FALSE)
    else{
      np$chart(useInteractiveGuideline = TRUE)
      np$save(path, standalone = TRUE)
      return(np)  
    }
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
    dataMat <- log2(dataComb[[2]] + 0.01)
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
    dataMat <- as.data.frame(dataMat)
    if(ncol(dataMat) < 10){
      library(GGally)
      pdf(paste(folder, '/pdfFiles/scatterPlot.pdf', sep = ''))
      gp <- ggpairs(dataMat)
      print(gp)
      dev.off()
    }
    else{
      gp <- ggplot() + geom_point(aes(x = S1, y = S2), data = dataMat)
      ggsave(paste(folder, '/pdfFiles/scatterPlot.pdf', sep = ''), gp)
    }
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
    dataMat <- log2(dataComb[[2]] + 0.01)
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
    dataMat1 <- as.data.frame(dataMat)
    dataMat1$IDs <- rownames(dataMat)
    dataMat1 <- data.table::melt(dataMat1, 'IDs')
    str(dataMat1)
    gp <- ggplot() + geom_boxplot(aes(x = variable, y = value), data = dataMat1)
    ggsave(paste(folder, '/pdfFiles/boxPlot.pdf', sep = ''), gp)
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
    dataMat <- log2(dataComb[[2]] + 0.01)
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
      cbind(as.data.frame(ppoints), design, paste('S', 1:ncol(dataMat), sep = ''))
    colnames(ppoints) <- c('PC1', 'PC2', 'PC3', 'Design', 'Samplename')
    ppoints <- as.data.frame(ppoints)
    return(list(ppoints, percent, folder))
  })
})

heteroModule <- reactive({
  if(input$ExpDesign == 'Not available' | (input$ExpDesign ==  'Multi-level' && input$MultiLevel == 'Identify most variable genes')){
    dataComb <- dataComb()
    str(dataComb)
    if(length(dataComb) < 7)
      data_spikein <- c()
    else
      data_spikein <- dataComb[[7]]
    prinStats <- Principalstats()
    path <- dataComb[[5]]
    withProgress(value = 1, message = 'Determine cell heterogeneity', {
      dataMat <- log2(dataComb[[2]] + 0.01)
      cors <- cor(dataMat)
      cors <- cors[lower.tri(cors)]
      fullGraph <- t(combn(ncol(dataMat), 2))
      fullGraphSel <- fullGraph[cors > quantile(cors, na.rm = TRUE)[4],]
      test <- idHetero(fullGraphSel)
      str(test)
      if(test$sig ==1){
        pp <- prinStats[[1]][,1:3]
        str(pp)
        foo <- ms.self.coverage(pp, taumin = 0.01, taumax = 1, gridsize = 100, plot.type = 'n')
        h <- select.self.coverage(foo)$select[1]
        test1 <- ms(pp, h, iter = 1000, plotms = 0)
        str(test1)
        if(length(unique(test1$cluster.label)) > 1){
          dir.create(paste(path, '/HeteroModule', sep = ''))
          withProgress(value = 1, message = paste('Characterize subpopulation ', input$DEmethod1, sep = ''),{
            write.csv(test1$cluster.label, paste(path, '/HeteroModule/subpopulationLabel.csv', sep = ''))
            for(i in 1:length(unique(test1$cluster.label))){
              group <- paste('C', ifelse(test1$cluster.label == i, 1, 2), sep = '')
              cat()
              if (input$DEmethod1 == 'XBSeq') {
                data_bg <- data.table::fread(input$file_bg$datapath, data.table = F)
                rownames(data_bg) <- data_bg[,1]
                data_bg <- data_bg[,-1]
                dataOut <- XBSeq.pfun(dataComb[[1]], data_bg, group, disp_method = input$SCVmethod, 
                                      sharing_mode = input$SharingMode, fit_type = input$fitType,
                                      paraMethod = input$ParamEst, spikeins = data_spikein, condition_sel = c())
              }
              else if (input$DEmethod1 == 'DESeq') {
                dataOut <- DESeq.pfun(dataComb[[1]], group, disp_method = input$SCVmethod,
                                      sharing_mode = input$SharingMode, 
                                      fit_type = input$fitType, spikeins = data_spikein, condition_sel = c())
              }
              else if (input$DEmethod1 == 'DESeq2') {
                dataOut <- DESeq2.pfun(
                  dataComb[[1]], group, cookcutoff = input$cooksCutoff,
                  fittype = input$fitType_DESeq2, test = input$Test, spikein = data_spikein, condition_sel = c())
              }
              else if (input$DEmethod1 == 'edgeR') {
                dataOut <- edgeR.pfun(dataComb[[1]], group, model.matrix(~group), spikeins = data_spikein, condition_sel = c())
                }
              else if (input$DEmethod1 == 'edgeR-robust') {
                dataOut <- edgeR_robust.pfun(dataComb[[1]], group, model.matrix(~group), spikeins = data_spikein, condition_sel = c())
                }
              else if (input$DEmethod1 == 'limma-voom') {
                dataOut <- limma_voom.pfun(dataComb[[1]], group, model.matrix(~group), spikeins = data_spikein, condition_sel = c())
                }
              else if (input$DEmethod1 == 'scde') {
                dataOut <- scde.pfun(dataComb[[1]], group, condition_sel = c())
              }
              else if (input$DEmethod1 == 'limma'){
                dataOut <- limma.pfun(dataComb[[1]], group, model.matrix(~0 + group), condition_sel = c())
                }
              else if (input$DEmethod1 == 'monocle'){
                dataOut <- monocle.pfun(dataComb[[1]], group, condition_sel = c())
              }
              else if (input$DEmethod1 == 'BPSC')
                dataOut <- BPSC.pfun(dataComb[[1]], group, model.matrix(~group), data_spikein, c(), 4)
              else if (input$DEmethod1 == 'MAST')
                dataOut <- MAST.pfun(dataComb[[1]], group, data_spikein, c())
              else if (input$DEmethod1 == 'EBSeq')
                dataOut <- EBSeq.pfun(data_obs, group, condition_sel)
              else if (input$DEmethod1 == 'ROTS')
                dataOut <- ROTS.pfun(data_obs, group, condition_sel)
              testStat <- dataOut[[4]]
              if(input$DEmethod1 == 'EBSeq')
                testStat$padj <- testStat[,3]
              else
                testStat$padj <- p.adjust(testStat[,3], method = input$padjust)
              write.csv(testStat, paste(path, '/HeteroModule/group', i,'vsOther.csv', sep =''), quote = F)
              }
          })
        }
        cols <- paste('C', test1$cluster.label, sep = '')
      }
      else
        cols <- rep('C1', ncol(dataMat))
    })
    return(cols)
  }
})


output$PrincipalComponent2d <- 
  renderChart({
    prinstat <- Principalstats()
    withProgress(value = 1, message = 'Generating plots: ', detail = 'PCA plot', {
      ppoints <- prinstat[[1]]
      if(input$ExpDesign == 'Not available'){
        ppoints$Design <- heteroModule()
      }
      percent <- prinstat[[2]]
      folder <- prinstat[[3]]
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
      #---- to fix bug of rCharts
      htmlTx <- readLines(path)
      index <- grep('//d3js.org/d3.v3.min.js', htmlTx)
      if(length(index)){
        htmlTx[index] <- "<script src='http://d3js.org/d3.v3.min.js' type='text/javascript'></script>"
        htmlTx[index + 1] <- "<script src='http://dimplejs.org/dist/dimple.v2.1.0.min.js' type='text/javascript'></script>"
        writeLines(htmlTx, path)
        }
      #---- fix bug complete
      gp <- ggplot() + geom_point(aes(x = PC1, y = PC2, color = Design), data = as.data.frame(ppoints))
      ggsave(paste(folder, '/pdfFiles/pcaPlot2d.pdf', sep = ''), gp)
      dp
      })
  })

output$PrincipalComponent3d <- threejs::renderScatterplotThree({
  library(scatterplot3d)
  prinstat <- Principalstats()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'PCA plot', {
    ppoints <- prinstat[[1]]
    if(input$ExpDesign == 'Not available'){
      ppoints$Design <- heteroModule()
    }
    if(input$ExpDesign == 'Not available'){
      ppoints$Design <- heteroModule()
    }
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
    if(length(unique(ppoints$Design)) > 8)
      colors <- rainbow_hcl(length(unique(ppoints$Design)))
    col <- colors[1:length(unique(ppoints$Design))]
    col <- rep(col, c(as.numeric(table(ppoints$Design))))
    scatter3d <- threejs::scatterplot3js(ppoints[,1], ppoints[,2], ppoints[,3], 
                                labels = paste('S',1:nrow(ppoints), sep = ''), 
                                axisLabels = c(xlab, ylab, zlab),
                                color = col,
                                renderer = 'canvas') # bg = '#ecf0f5'
    htmlwidgets::saveWidget(scatter3d, 'pcaplot.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    if(file.exists(paste(path, 'pcaplot.html', sep = '')))
      file.remove(paste(path, 'pcaplot.html', sep = ''))
    file.copy('./pcaplot.html', path)
    file.remove('./pcaplot.html')
    pdf(paste(folder, '/pdfFiles/pcaPlot3d.pdf', sep = ''))
    scatterplot3d(ppoints[,1], ppoints[,2], ppoints[,3], color = col, xlab = xlab, ylab = ylab, zlab = zlab, pch = 19)
    dev.off()
    return(scatter3d)
    })
})

output$value_Exprscut <- renderPrint({
  input$Exprscut
})

output$value_Corrcut <- renderPrint({
  input$Corrcut
})

output$forceNetworkGene <- networkD3::renderForceNetwork({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Gene interaction network', {
    library(igraph)
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep = '')
    mean.gene <- apply(dataMat, 1, mean)
    meanCutQ <- switch(input$Exprscut,
                       "0% quantile" = 1,
                       "25% quantile" = 2,
                       "50% quantile" = 3,
                       "75% quantile" = 4,
                       "100% quantile" =5)
    dataMat <- dataMat[mean.gene > quantile(mean.gene, na.rm = TRUE)[meanCutQ],]
    if (length(dataMat) == 0 ||
        dim(dataMat)[1] < 10)
      return(NULL)
    else
    {
      combs <- combn(1:nrow(dataMat), 2)
      cors <- cor(t(dataMat), method = 'spearman')
      MisLinks <-
        data.frame(
          source = combs[1,],
          target = combs[2,],
          value = cors[lower.tri(cors)]
        )
      index <- which(abs(MisLinks$value) > input$Corrcut)
      MisLinks <- MisLinks[index,]
      ranks <- data.table::frank(unlist(MisLinks[,1:2]), ties.method = 'dense')
      MisLinks$source <- ranks[1:nrow(MisLinks)] -1 
      MisLinks$target <- ranks[(nrow(MisLinks) + 1):(2*nrow(MisLinks))] -1
      name <- rownames(dataMat)[sort(unique(c(combs[,index])))]
      MisNodes <- data.frame(
        name = name,
        group = rep(1, length(name)),
        size = rep(15, length(name)), 
        stringsAsFactors = F
      )
      forceNet <- networkD3::forceNetwork(
        Links = MisLinks, Nodes = MisNodes, Source = "source",
        Target = "target", Value = "value", NodeID = "name",
        Group = "group", opacity = 0.4, bounded = FALSE
      )
      htmlwidgets::saveWidget(forceNet, 'forceNet.html', background = 'none')
      path <- paste(folder, '/htmlFiles/', sep = '')
      if(file.exists(paste(path, './forceNet.html', sep = '')))
        file.remove(paste(path, './forceNet.html', sep = ''))
      file.copy('./forceNet.html', path)
      file.remove('./forceNet.html')
      edgeList <- MisLinks[,1:2]
      edgeList$source <- MisNodes$name[edgeList$source + 1]
      edgeList$target <- MisNodes$name[edgeList$target + 1]
      ig <- graph_from_edgelist(as.matrix(edgeList), directed = FALSE)
      pdf(paste(folder, '/pdfFiles/network.pdf', sep = ''))
      plot(ig, vertex.size = 10, vertex.label.cex = 0.5,layout=layout_with_fr, vertex.shape = 'square')
      dev.off()
      return(forceNet)
    }
    })

})

output$DEtable <- DT::renderDataTable({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'DE table', {
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    dataMat <- as.data.frame(dataMat)
    dataMat1 <- dataComb[[4]]
    if(input$DEmethod == 'EBSeq' || (input$DEmethod == 'Brennecke_2013' && input$DEmethod1 == 'EBSeq'))
      p_adjust1 <- dataMat1[,3]
    else
      p_adjust1 <- p.adjust(dataMat1[,3], method = input$padjust)
    p_adjust1[is.na(p_adjust1)] <- 1
    meanCutQ <- switch(input$log2bmcutoff,
                       "0% quantile" = 1,
                       "25% quantile" = 2,
                       "50% quantile" = 3,
                       "75% quantile" = 4,
                       "100% quantile" =5)
    DE_index <-
      which(
        log2(dataMat1[,1] + 0.01) > quantile(log2(dataMat1[,1] + 0.01), na.rm = TRUE)[meanCutQ] &
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
    temp <- DT::datatable(dataMat2, options = list(pageLength = 5))
    htmlwidgets::saveWidget(temp, 'DEdatatable.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./DEdatatable.html', path)
    file.remove('./DEdatatable.html')
    # ------ fix potential bug of DT
    htmlTx <- readLines(path)
    index <- grep('"\\\\&quot;display\\\\&quot;"', htmlTx)
    if(length(index))
    {
      htmlTx <- gsub('"\\\\&quot;display\\\\&quot;"', '\"display\"', htmlTx)
      writeLines(htmlTx, path)
    }
    # ------ fix complete
    temp
      })
})

output$DEheatmap <- d3heatmap::renderD3heatmap({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'DE heatmap', {
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    dataMat <- as.data.frame(dataMat)
    dataMat1 <- dataComb[[4]]
    if(input$DEmethod == 'EBSeq' || (input$DEmethod == 'Brennecke_2013' && input$DEmethod1 == 'EBSeq'))
      p_adjust1 <- dataMat1[,3]
    else
      p_adjust1 <- p.adjust(dataMat1[,3], method = input$padjust)
    p_adjust1[is.na(p_adjust1)] <- 1
    meanCutQ <- switch(input$log2bmcutoff,
                       "0% quantile" = 1,
                       "25% quantile" = 2,
                       "50% quantile" = 3,
                       "75% quantile" = 4,
                       "100% quantile" =5)
    DE_index <-
      which(
        log2(dataMat1[,1] + 0.01) > quantile(log2(dataMat1[,1] + 0.01), na.rm = TRUE)[meanCutQ] &
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
    h1 <- d3heatmap::d3heatmap(dataMat2, scale = "row", colors = colorRampPalette(c("blue","white","red"))(1000), Colv = FALSE)
    htmlwidgets::saveWidget(h1, 'DEheatmap.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./DEheatmap.html', path)
    file.remove('./DEheatmap.html')
    pdf(paste(folder, '/pdfFiles/', 'DEheatmap.pdf', sep = ''))
    heatmap.my(as.matrix(dataMat2))
    dev.off()
    h1
  })
})

output$MAplot <- metricsgraphics::renderMetricsgraphics({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'MAplot', {
    dataMat <- dataComb[[4]]
    folder <- dataComb[[5]]
    colnames(dataMat) <- c('baseMean', 'log2FoldChange', 'p_adjust')
    if(input$DEmethod == 'EBSeq' || (input$DEmethod == 'Brennecke_2013' && input$DEmethod1 == 'EBSeq'))
      p_adjust1 <- dataMat[,3]
    else
      p_adjust1 <- p.adjust(dataMat[,3], method = input$padjust)
    p_adjust1[is.na(p_adjust1)] <- 1
    meanCutQ <- switch(input$log2bmcutoff,
                       "0% quantile" = 1,
                       "25% quantile" = 2,
                       "50% quantile" = 3,
                       "75% quantile" = 4,
                       "100% quantile" =5)
    col <-
      with(
        data = dataMat, ifelse(
          log2(baseMean + 0.01) > quantile(log2(baseMean + 0.01), na.rm = TRUE)[meanCutQ] &
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
      metricsgraphics::mjs_plot(dataMat, baseMean, log2FoldChange, decimals = 6) %>%
      metricsgraphics::mjs_point(
        color_accessor = col, color_range = color_rg, color_type = "category", x_rug =
          TRUE, y_rug = TRUE
      ) %>%
      metricsgraphics::mjs_add_baseline(y_value = 0, label = 'baseline') %>%
      metricsgraphics::mjs_labs(x_label = xlab, y_label = "Log2 fold change") %>%
      metricsgraphics::mjs_add_mouseover("function(d) {
                $('{{ID}} svg .mg-active-datapoint')
                    .text('Gene Name: ' +  d.point.Genename + ',' + ' Log2 intensity: ' + d.point.baseMean + ',' + ' Log2 fold change: ' + d.point.log2FoldChange);
                 }")
    htmlwidgets::saveWidget(mp, file = 'MAplot.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./MAplot.html', path)
    file.remove('./MAplot.html')
    dataMat = subset(dataMat, baseMean != 0)
    y = dataMat$log2FoldChange
    ylim = c(-1, 1) * quantile(abs(y[is.finite(y)]), probs = 0.99) * 
      1.1
    shape = as.factor(ifelse(y < ylim[1], 6, ifelse(y > ylim[2], 2, 
                                            16)))
    dataMat$log2FoldChange = pmax(ylim[1], pmin(ylim[2], y))
    dataMat$shape <- shape
    gp <- ggplot() + geom_point(aes(x = baseMean, y = log2FoldChange, color = col, shape = shape), data = dataMat) + ylim(ylim) +
      geom_hline(yintercept = 0, colour = 'red3', size = 1) + 
      scale_color_manual(values = color_rg) +
      scale_x_log10() + labs(x = xlab, y = "Log2 fold change") +
      guides(shape = 'none')
    ggsave(paste(folder, '/pdfFiles/', 'MAplot.pdf', sep = ''), gp)
    mp
    })
})

output$DispersionPlot <- renderChart({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'Dispersion plot', {
    Dispersion <- dataComb[[3]]
    folder <- dataComb[[5]]
    Dispersion$baseMean <- dataComb[[4]][,1] + 0.01
    if(input$DEmethod == 'DESeq2'){
      Dispersion1 <- stack(Dispersion[,c(1,3)])
      colnames(Dispersion1) <- c('Disp', 'Type')
      Dispersion1$baseMean <- rep(Dispersion$baseMean, times = 2)
      Dispersion1$Dispfit <- rep(Dispersion$dispFit, times = 2)
      Dispersion1$GeneName <- rep(rownames(dataComb[[2]]), times = 2)
      rp <-
        rPlot(
          Disp ~ baseMean, data = Dispersion1, color = 'Type', type = 'point', size = list(const = 2),
          tooltip = "#!function(item){ return 'x: ' + item.baseMean + ',' +
          ' y: ' + item.Disp + ',' + ' GeneName: ' + item.GeneName  + ',' + ' Type: ' + item.Type}!#"
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
      gp <- ggplot(data = Dispersion1) + geom_point(aes(x = baseMean, y = Disp, color = Type)) + 
        geom_line(aes(x = baseMean, y = Dispfit), color = 'red') + scale_x_log10()
    }
    if(input$DEmethod == 'DESeq' | input$DEmethod == 'XBSeq')
    {
      Dispersion$GeneName <- rownames(dataComb[[2]])
      colnames(Dispersion) <- c('PerGeneEst', 'FittedDispEst', 'baseMean', 'GeneName')
      rp <-
        rPlot(
          PerGeneEst ~ baseMean, data = Dispersion, type = 'point', size = list(const = 2),
          tooltip = "#!function(item){ return 'x: ' + item.baseMean + ',' +
          ' y: ' + item.PerGeneEst + ',' + ' GeneName: ' + item.GeneName}!#"
        )
      rp$guides("{x: { scale: {type: 'log'}}}")
      rp$layer(
        y = 'FittedDispEst', type = 'line', color = list(const = 'red'),
        copy_layer = T
      )
      rp$addParams(dom = "DispersionPlot")
      gp <- ggplot(data = Dispersion) + geom_point(aes(x = baseMean, y = PerGeneEst)) + 
        geom_line(aes(x = baseMean, y = FittedDispEst), color = 'red') + scale_x_log10()
    }
    if(input$DEmethod == 'edgeR'){
      Dispersion$GeneName <- rownames(dataComb[[2]])
      colnames(Dispersion) <- c('CommonDisp', 'TagwiseDisp', 'FittedDisp', 'baseMean', 'GeneName')
      rp <-
        rPlot(
          TagwiseDisp ~ baseMean, data = Dispersion, type = 'point', size = list(const = 2),
          tooltip = "#!function(item){ return 'x: ' + item.baseMean + ',' +
          ' y: ' + item.TagwiseDisp + ',' + ' GeneName: ' + item.GeneName}!#"
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
      gp <- ggplot(data = Dispersion) + geom_point(aes(x = baseMean, y = TagwiseDisp)) + 
        geom_line(aes(x = baseMean, y = FittedDisp), color = 'red') + 
        scale_x_log10() +
        geom_line(aes(x = baseMean, y = CommonDisp), color = 'blue')
    }
    if(input$DEmethod == 'edgeR-robust'){
      Dispersion$GeneName <- rownames(dataComb[[2]])
      colnames(Dispersion) <- c('TagwiseDisp', 'FittedDisp', 'baseMean', 'GeneName')
      rp <-
        rPlot(
          TagwiseDisp ~ baseMean, data = Dispersion, type = 'point', size = list(const = 2),
          tooltip = "#!function(item){ return 'x: ' + item.baseMean + ',' +
        ' y: ' + item.TagwiseDisp + ',' + ' GeneName: ' + item.GeneName}!#"
        )
      rp$guides("{x: { scale: {type: 'log'}}}")
      rp$layer(
        y = 'FittedDisp', type = 'line', color = list(const = 'red'),
        copy_layer = T
      )
      rp$addParams(dom = "DispersionPlot")
      gp <- ggplot(data = Dispersion) + geom_point(aes(x = baseMean, y = TagwiseDisp)) + 
        geom_line(aes(x = baseMean, y = FittedDisp), color = 'red') + 
        scale_x_log10()
    }
    path <- paste(folder, '/htmlFiles/DispersionPlot.html', sep = '')
    rp$save(path, standalone = TRUE)
    ggsave(paste(folder, '/pdfFiles/dispersionPlot.pdf', sep = ''), gp)
    rp
    })
})

output$HVGtable <- DT::renderDataTable({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'HVG table', {
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    dataMat <- as.data.frame(dataMat)
    DE_index <- dataComb[[4]]
    if (length(DE_index) == 0)
      dataMat2 <- c()
    else{
      dataMat2 <- dataMat[DE_index,]
    }
    write.csv(dataMat, paste(folder, '/NormData.csv', sep = ''), quote = F)
    write.csv(dataMat2, paste(folder, '/HVGData.csv', sep = ''), quote = F)
    temp <- DT::datatable(dataMat2, options = list(pageLength = 5))
    htmlwidgets::saveWidget(temp, 'HVGdatatable.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./HVGdatatable.html', path)
    file.remove('./HVGdatatable.html')
    temp
  })
})

output$HVGheatmap <- d3heatmap::renderD3heatmap(({
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'HVG heatmap', {
    dataMat <- log2(dataComb[[2]] + 0.01)
    folder <- dataComb[[5]]
    dataMat <- as.data.frame(dataMat)
    DE_index <- dataComb[[4]]
    if (length(DE_index) <= 2){
      htmltools::save_html(c(), 'HVGheatmap.html')
      path <- paste(folder, '/htmlFiles/', sep = '')
      file.copy('./HVGheatmap.html', path)
      file.remove('./HVGheatmap.html')
      return(NULL)
    }
    dataMat2 <-dataMat[DE_index,]
    colnames(dataMat2) <- paste('S', 1:ncol(dataMat), sep = '')
    h1 <- d3heatmap::d3heatmap(dataMat2, scale = "row", colors = colorRampPalette(c("blue","white","red"))(1000), Colv = FALSE)
    htmlwidgets::saveWidget(h1, 'HVGheatmap.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./HVGheatmap.html', path)
    file.remove('./HVGheatmap.html')
    pdf(paste(folder, '/pdfFiles/', 'HVGheatmap.pdf', sep = ''))
    heatmap.my(as.matrix(dataMat2))
    dev.off()
    h1
  })
}))

output$HVGplot <- metricsgraphics::renderMetricsgraphics({
  if (is.null(input$file_obs))
    return(NULL)
  dataComb <- dataComb()
  withProgress(value = 1, message = 'Generating plots: ', detail = 'HVG plot', {
    dataMat <- dataComb[[2]]
    CV <- dataComb[[3]]
    HVG_ind <- dataComb[[4]]
    col <- rep('Non-HVG', length(CV))
    col[HVG_ind] <- 'HVG'
    col <- as.factor(col)
    folder <- dataComb[[5]]
    dataMat_mean <- apply(log2(dataMat + 0.01), 1, mean)
    dataMat1 <- data.frame(
      dataMat_mean <- dataMat_mean,
      CV <- CV,
      col <- col,
      geneName <- rownames(dataMat)
    )
    colnames(dataMat1) <- c('baseMean', 'CV', 'Col')
    if(length(unique(col)) == 1)
      color_rg <- 'grey32'
    else
      color_rg <- c('red', 'grey32')
    mp <-
      metricsgraphics::mjs_plot(dataMat1, baseMean, CV, decimals = 6) %>%
      metricsgraphics::mjs_point(
        color_accessor = Col, color_range = color_rg, color_type = "category", x_rug =
          TRUE, y_rug = TRUE
      ) %>%
      metricsgraphics::mjs_labs(x_label = 'Log2 expression intensity', y_label = "Coeffcient of variation") %>%
      metricsgraphics::mjs_add_mouseover("function(d) {
                        $('{{ID}} svg .mg-active-datapoint')
                        .text('Gene Name: ' +  d.point.geneName + ',' + ' Log2 intensity: ' + d.point.baseMean + ',' + ' CV: ' + d.point.CV);
  }")
    htmlwidgets::saveWidget(mp, file = 'HVGplot.html', background = 'none')
    path <- paste(folder, '/htmlFiles/', sep = '')
    file.copy('./HVGplot.html', path)
    file.remove('./HVGplot.html')
    gp <- ggplot() + geom_point(aes(x = baseMean, y = CV, color = Col), data = dataMat1) +
      scale_colour_manual(values = color_rg) + labs(x = 'Log2 expression intensity', y = "Coeffcient of variation")
    ggsave(paste(folder, '/pdfFiles/', 'HVGplot.pdf', sep = ''), gp)
    mp
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


