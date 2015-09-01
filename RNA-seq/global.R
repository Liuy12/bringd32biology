library(slidify)
library(slidifyLibraries)
library(googleAuthR)
library(shiny)
library(shinydashboard)
library(googleVis)
library(mailR)
library(d3heatmap)
library(metricsgraphics)
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

#Texts
WEBTITLE <- "RNA-seq Viz"

#Files
userFile <- "data/user.csv"
emailFile <- "data/email.txt"


Logged = FALSE
Group=""
userData <- read.csv(userFile,header=T,as.is=T)
GLOBALDATA <- reactiveValues(userData=userData)

# gar_shiny_getAuthUrl <- 
#   function(redirect.uri,
#            state = getOption("googleAuthR.securitycode"),
#            client.id     = getOption("googleAuthR.webapp.client_id"),
#            client.secret = getOption("googleAuthR.webapp.client_secret"),
#            scope         = getOption("googleAuthR.scopes.selected")) {
#     
#     scopeEnc <- paste(scope, sep='', collapse=' ')
#     
#     ## httr friendly version
#     url <- httr::modify_url(
#       httr::oauth_endpoints("google")$authorize,
#       query = list(response_type = "code",
#                    client_id = client.id,
#                    redirect_uri = redirect.uri,
#                    scope = scopeEnc,
#                    state = state,
#                    access_type = "online",
#                    approval_prompt = "auto"))
#     message("Auth Token URL: ", url)
#     url
#   }
# 
# 
# gar_shiny_getUrl <- function(session){
#   
#   if(!is.null(session)){
#     pathname <- session$clientData$url_pathname
#     ## hack for shinyapps.io
#     if(session$clientData$url_hostname == "internal.shinyapps.io"){
#       split_hostname <- strsplit(pathname, "/")[[1]]
#       hostname <-  paste(split_hostname[2],"shinyapps.io", sep=".")
#       pathname <- paste0("/",split_hostname[3],"/")
#       
#     } else {
#       hostname <- session$clientData$url_hostname
#     }
#     
#     url <- paste0(session$clientData$url_protocol,
#                   "//",
#                   hostname,
#                   ifelse(hostname == "127.0.0.1",
#                          ":",
#                          pathname),
#                   session$clientData$url_port)
#     #     message("Shiny URL detected as: ", url)
#     url
#   } else {
#     NULL
#   }
#   
#   
# }




#Emails
emailList<-readLines(emailFile)
sendEmailSign<-TRUE

SMTP_FROM = "Yuanhang <yuanhangliu.ok@gmail.com>"
SMTP_SETTINGS = list(host.name='aspmx.l.google.com',
		port='2525',
		user.name='yuanhangliu.ok@gmail.com',
		passwd='')
SMTP_AUTHENTICATE=TRUE

sendEmail <- function(subject="New Assignment",body="New Assignment Content",to=emailList) {
	send.mail(from = SMTP_FROM,
			to = to,
			subject = subject,
			body = body,
			smtp = SMTP_SETTINGS,
			authenticate = SMTP_AUTHENTICATE,
			send = TRUE)
}

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
    NormCount = counts(dse, normalized = TRUE)
    dataout <- cbind(NormCount, as.data.frame(res[, c(1,2,5)]))
    colnames(dataout) <- c(colnames(counts), 'baseMean', 'Log2 Fold change', 'p value')
    write.csv(dataout, 'www/report/TestStat.csv', quote = F)
    list(
      RawCount = counts,
      NormCount = NormCount,
      Dispersion = as.data.frame(mcols(dse)[,4:6]),
      TestStat = as.data.frame(res[, c(1,2,5)])
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

renderChart3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    rChart_ <- func()
    cht_style <- sprintf("<style>.rChart {width: %spx; height: %spx} </style>",
                         rChart_$params$width, rChart_$params$height)
    cht <- paste(capture.output(rChart_$print()), collapse = '\n')
    fcht <- paste(c(cht_style, cht), collapse = '\n')
    fcht <- gsub("\\\\", "\\", fcht, fixed=T)
    HTML(fcht)
  }
}



