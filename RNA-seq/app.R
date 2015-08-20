library(d3heatmap)
library(shiny)
library(networkD3)
library(DT)
library(rCharts)
library(shinythemes)
library(data.table)
library(dplyr)
library(DESeq)
library(DESeq2)
library(XBSeq)
library(edgeR)
library(limma)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidRow(column(12, tags$header(strong(HTML("<p align = 'center'>RNA-seq Data: Bringing <span style='color: red;'>D3</span> visualization to RNA-seq!")), style = "font-size: 50px; 
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
                       label = "Please choose either to turn on or off cooks distance cutoff",
                       choices = c('on',
                                   'off'),
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
      verbatimTextOutput("fccutoff"),
      numericInput("log2bmcutoff", label = "Please set a cutoff for log2 expression intensity (Usually can be determined from density plot)", 
                value = 5, min = 1
      ),
      verbatimTextOutput("log2bmcutoff"),
      actionButton('DEstart', label = 'Start analysis!'),
      textOutput("DEstart"),
      checkboxGroupInput("checkGroup", 
                         label = h3("Which graphs would you like to save?"), 
                         choices = list("Heatmap" = 1, 
                                        "Kernel Density Estimation" = 2, 
                                        "Scatter Plot" = 3),
                         selected = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("Table")),
#        tabPanel("Heatmap", d3heatmapOutput('Heatmap')), 
        tabPanel("Kernel Density Estimation", showOutput("Density", "nvd3")), 
        tabPanel("Scatter Plot", sidebarLayout(
          sidebarPanel(
            textInput("text_S1", label = h5("Enter first sample name (For example, S1)"), 
                      value = "S1"
            ),
            verbatimTextOutput("value_S1"),
            textInput("text_S2", label = h5("Enter second sample name (For example, S2)"), 
                      value = "S2"
            ),
            verbatimTextOutput("value_S2")
          ),
          mainPanel(showOutput("ScatterPlot", "highcharts")))
        ),
        tabPanel("Boxplot", plotOutput("Boxplot")),
        tabPanel("Principal Component", sidebarLayout(
          sidebarPanel(
            selectizeInput("cvCutoff", 
                           label = 'Please select a cutoff for cv (coefficient of variation)',
                           choices = c(0.1, 0.3, 0.5)
            ),
            verbatimTextOutput("value_cvcutoff"),
            selectizeInput("clusterMethod", 
                           label = 'Please select a method for clustering (pca or mds)',
                           choices = c('pca', 'mds')
            ),
            verbatimTextOutput("value_clustermethod")
          ),
          mainPanel(showOutput("PrincipalComponent", "nvd3")))
        ),
        tabPanel("Gene interaction network", sidebarLayout(
          sidebarPanel(
            sliderInput("Exprscut", "Expression level cutoff", 
                        min=0, max=20, step = 2, value=8
            ),
            verbatimTextOutput("value_Exprscut"),
            sliderInput("Corrcut", "Correlation cutoff", 
                        min=0, max=1, step = 0.1, value=0.9
            ),
            verbatimTextOutput("value_Corrcut")
          ),
          mainPanel(forceNetworkOutput("forceNetworkGene")))
        ),
        tabPanel("DE Table", dataTableOutput('DEtable')),
        tabPanel("MAplot", showOutput("MAplot", "nvd3")),
        #tabPanel("DE Heatmap", d3heatmapOutput('DEheatmap')),
        tabPanel("Dispersion plot", showOutput("DispersionPlot", "nvd3")),
        conditionalPanel(condition = "input$DEmethod == 'XBSeq'",
                         tabPanel("XBSeq plot", showOutput("XBSeqPlot", "nvd3"))
        )
      )
    )
  ))



DESeq2_pfun <-
  function(counts, group, design = NULL, cookcutoff, fittype, test)
  {   
    options(mc.cores = min(10, detectCores()))
    colData <- data.frame(group)
    dse <- DESeqDataSetFromMatrix(countData = counts, colData = colData, design = ~ group)
    colData(dse)$group <- as.factor(colData(dse)$group)
    if(test == 'LRT')
      dse <- DESeq(dse, test = 'LRT', fitType = fittype, reduced = ~1)
    else
      dse <- DESeq(dse, test = 'Wald', fitType = fittype)
    if(cookcutoff == 'on')
      res <- results(dse)
    else
      res <- results(dse, cooksCutoff = FALSE)
    list(
      RawCount = counts,
      NormCount = counts(dse, normalized = TRUE),
      Dispersion = mcols(dse)[,4:6],
      TestStat = res[, c(1,2,5)]
    )
  }

DESeq_pfun <-
  function(counts, group, design = NULL, mc.cores = 4)
  {   
    ## implement DESeq using pooled method to estimate dispersion ##
    library(DESeq)
    de <- newCountDataSet(counts, group)
    de <- estimateSizeFactors(de)
    de <- estimateDispersions(de, method = "pooled",fitType='local')
    res <- nbinomTest(de, levels(group)[1], levels(group)[2])
    cbind(pval = res$pval, padj = res$padj)
  }

edgeR.pfun <-
  function(counts, group, design = NULL, mc.cores = 4, prior.df=10)
  {
    ## edgeR standard pipeline ##
    library(edgeR)
    d <- DGEList(counts = counts, group = group )
    d <- calcNormFactors(d)
    d <- estimateGLMCommonDisp(d, design = design)
    d <- estimateGLMTrendedDisp(d,design=design)
    d <- estimateGLMTagwiseDisp(d, design = design, prior.df = prior.df)
    f <- glmFit(d, design = design)
    lr <- glmLRT(f, coef=2)
    pval = lr$table$PValue
    padj = p.adjust(pval, "BH")
    logfc = lr$table$logFC
    cbind(pval = pval, padj = padj, logfc = logfc)
  }

edgeR_robust.pfun <-
  function(counts, group, design = NULL, mc.cores = 4, prior.df=10)
  {   
    ## edgeR-robsut pipeline ##
    library(edgeR)
    d <- DGEList(counts = counts, group = group )
    d <- calcNormFactors(d)
    dw <- estimateGLMRobustDisp(d,design=design, prior.df=prior.df, maxit = 6)
    fw <- glmFit(dw, design=design)
    lrw <- glmLRT(fw,coef=2)
    pval = lrw$table$PValue
    padj = p.adjust(pval, "BH")
    logfc = lrw$table$logFC
    cbind(pval = pval, padj = padj, logfc=logfc)
  }


limma_voom.pfun <-
  function(counts, group, design = NULL, mc.cores = 4) 
  {   
    ## limma voom pipeline ##
    library(limma)
    nf <- calcNormFactors(counts)
    y <- voom(counts, design, plot=FALSE, lib.size = colSums(counts)*nf)
    fit <- lmFit(y, design)
    fit <- eBayes(fit)
    pval <- topTable(fit,coef=2,n=nrow(counts), sort.by = "none")$P.Value
    padj <- topTable(fit,coef=2,n=nrow(counts), sort.by = "none")$adj.P.Val
    cbind(pval = padj, padj = padj)
  }



server <- function(input, output) {
  design <- reactive({
    inFile <- input$file_design
    if (is.null(inFile))
      return(NULL)
    fread(inFile$datapath, data.table=F)
  })
  
  StartMessage <- eventReactive(input$DEstart, {
    "Please wait, this might take a while"
  })
  
  output$DEstart <- renderText({StartMessage()})
  
  dataComb <- eventReactive(input$DEstart, {
    inFile <- input$file_obs
    if (is.null(inFile))
      return(NULL)
    data_obs <- fread(inFile$datapath, data.table=F)
    rownames(data_obs) <- data_obs[,1]
    data_obs <- data_obs[,-1]
    if(input$DEmethod == 'XBSeq'){
      
    }
    else if(input$DEmethod == 'DESeq'){
      
    }
    else if(input$DEmethod == 'DESeq2'){
      group <- design()
      group <- as.factor(group$Design)
      DESeq2_pfun(data_obs, group, cookcutoff = input$cooksCutoff, 
                  fittype = input$fitType_DESeq2, test = input$Test)
    }
    else if(input$DEmethod == 'edgeR'){
      
    }
    else if(input$DEmethod == 'edgeR-robust'){
      
    }
    else if(input$DEmethod == 'limma-voom'){
      
    }
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
  
  output$pcutoff <- renderPrint({as.numeric(input$pcutoff)})
  
  output$fccutoff <- renderPrint({as.numeric(input$fccutoff)})
  
  output$log2bmcutoff <- renderPrint({input$log2bmcutoff})
  
  output$Table <- renderDataTable({
    if (is.null(input$file_obs))
      return(NULL)
    dataComb <- dataComb()
    dataMat <- log2(dataComb[[2]])
    dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
    
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    datatable(dataMat, options = list(pageLength = 5))
  })
  
#   output$Heatmap <- renderD3heatmap({
#     if (is.null(input$file_obs))
#       return(NULL)
#     dataComb <- dataComb()
#     dataMat <- log2(dataComb[[2]])
#     dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
#     colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
#     d3heatmap(as.data.frame(dataMat), scale="row", labCol = NULL, anim_duration = 0, colors=colorRampPalette(c("blue","white","red"))(1000))
#   })
  
  
  output$Density <- renderChart({
    dataComb <- dataComb()
    dataMat <- log2(dataComb[[2]])
    dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
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
    np$chart(useInteractiveGuideline = TRUE)
    np$xAxis(axisLabel = 'Log2 intensity')
    np$yAxis(axisLabel = 'Density')
    return(np)
  })
  
  output$value_S1 <- renderPrint({input$text_S1})
  
  output$value_S2 <- renderPrint({input$text_S2})
  
  output$ScatterPlot <- renderChart({
    dataComb <- dataComb()
    dataMat <- log2(dataComb[[2]])
    dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    dataMat <- as.data.frame(dataMat)
    hp <- hPlot(x= input$text_S1, y = input$text_S2, data = dataMat, type = 'scatter', radius = 3)
    hp$addParams(dom = "ScatterPlot")
    hp$colors('black')
    return(hp)
  })
  
  output$value_cvcutoff <- renderPrint({input$cvCutoff})
  
  output$value_clustermethod <- renderPrint({input$clusterMethod})
  
  output$PrincipalComponent <- renderChart({
    colors <- c('#00FFFF', '#FFE4C4', '#D2691E', '#6495ED', '#9932CC', '#8B0000', 
                '#FF00FF', '#FFD700')
    dataComb <- dataComb()
    dataMat <- log2(dataComb[[2]])
    dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
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
    np$chart(color = colors[1:length(unique(design$Design))])
    np$xAxis(axisLabel = 'PC1')
    np$yAxis(axisLabel = 'PC2')
    np$chart(sizeRange = c(50,50))
    return(np)
  })
  
  output$value_Exprscut <- renderPrint({input$Exprscut})
  
  output$value_Corrcut <- renderPrint({input$Corrcut})
  
  output$forceNetworkGene <- renderForceNetwork({
    if (is.null(input$file_obs))
      return(NULL)
    dataComb <- dataComb()
    dataMat <- log2(dataComb[[2]])
    dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
    colnames(dataMat) <- paste('S', 1:ncol(dataMat), sep='')
    mean.gene <- apply(dataMat, 1, mean)
    dataMat <- dataMat[mean.gene > input$Exprscut, ]
    if(length(dataMat) == 0 | length(dataMat) == ncol(dataComb[[2]]))
      return(NULL)
    else
    {
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
    }
  })
  
  output$DEtable <- renderDataTable({
    if (is.null(input$file_obs))
      return(NULL)
    dataComb <- dataComb()
    dataMat <- log2(dataComb[[2]])
    dataMat[is.na(dataMat) | is.infinite(dataMat)] <- 0
    dataMat1 <- dataComb[[4]]
    p_adjust <- p.adjust(dataMat1[,3], method = input$padjust)
    DE_index <- which(log2(dataMat1[,1]) > as.numeric(input$log2bmcutoff) & 
                      dataMat1[,2] > log2(as.numeric(input$fccutoff)) &
                      p_adjust < as.numeric(input$pcutoff))
    if(length(DE_index) == 1)
      return(datatable(data.frame()), options = list(pageLength = 5))
    
    dataMat <- cbind(dataMat[DE_index,], dataMat1[,2], p_adjust)
    colnames(dataMat) <- c(paste('S', 1:ncol(dataMat), sep=''), 'Log2 fold change', 'p adjusted value')
    datatable(dataMat, options = list(pageLength = 5))
    })
  
  output$MAplot <- renderChart({
    dataComb <- dataComb()
    dataMat <- dataComb[[4]]
    p_adjust <- p.adjust(dataMat[,3], method = input$padjust)
    col <- with(data = dataMat, ifelse(log2(baseMean) > as.numeric(input$log2bmcutoff) & 
                                         log2FoldChange > log2(as.numeric(input$fccutoff)) &
                                         p_adjust < as.numeric(input$pcutoff),
                                       "G1", "G2"))
    dataMat <- cbind(dataMat, col)
    np <- nPlot(log2FoldChange ~ baseMean, data = dataMat, group= 'col', type = 'scatterChart')
    np$addParams(dom = "PrincipalComponent")
    np$chart(color = c('red', 'grey32'))
    np$xAxis(axisLabel = 'Expression intensity',
             tickFormat = "#!function (x) {tickformat = [1,10,100,1000,10000,'100k'];return tickformat[x];}!#")
    np$yAxis(axisLabel = 'Log2 fold change')
    np$chart(sizeRange = c(20,20))
    return(np)
  })
}



shinyApp(ui, server)