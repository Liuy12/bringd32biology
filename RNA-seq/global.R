library(slidify)
library(slidifyLibraries)
#library(googleAuthR)
library(shiny)
library(shinydashboard)
#library(googleVis)
#library(mailR)
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
library(rga)
library(threejs)
library(scde)
# library(BASiCS) too slow
#library(samr) Not working
library(RUVSeq)
library(statmod)
library(monocle)


#Emails
# emailList<-readLines(emailFile)
# sendEmailSign<-TRUE
# 
# SMTP_FROM = "Yuanhang <yuanhangliu.ok@gmail.com>"
# SMTP_SETTINGS = list(host.name='aspmx.l.google.com',
# 		port='2525',
# 		user.name='yuanhangliu.ok@gmail.com',
# 		passwd='')
# SMTP_AUTHENTICATE=TRUE
# 
# sendEmail <- function(subject="New Assignment",body="New Assignment Content",to=emailList) {
# 	send.mail(from = SMTP_FROM,
# 			to = to,
# 			subject = subject,
# 			body = body,
# 			smtp = SMTP_SETTINGS,
# 			authenticate = SMTP_AUTHENTICATE,
# 			send = TRUE)
# }

XBSeq_pfun <- 
  function(counts, bgcounts, group, disp_method, sharing_mode, fit_type, paraMethod, spikeins){
    XB <- XBSeqDataSet(counts, bgcounts, group)
    XB <- estimateRealCount(XB)
    if(!is.null(spikeins)){
      counts <- rbind(spikeins, counts(XB))
      sizeFactors(XB) <- DESeq2::estimateSizeFactorsForMatrix(counts, controlGenes = 1:nrow(spikeins))
    }
    else
      XB <- estimateSizeFactors(XB)
    XB <- estimateSCV(XB, method = disp_method, sharingMode = sharing_mode, fitType = fit_type)
    Teststas <- XBSeqTest( XB, levels(group)[1L], levels(group)[2L], method =paraMethod)
    Disp <- XBSeq::fitInfo(XB)
    Dispersion <- data.frame(
      PerGeneEst <- Disp$perGeneSCVEsts,
      FittedDispEst <- Disp$fittedSCVEsts
    )
    colnames(Dispersion) <- c('PerGeneEst', 'FittedDispEst')
    list(
      RawCount = counts,
      NormCount = counts(XB, normalized = TRUE),
      Dispersion = Dispersion,
      TestStat = Teststas[,c(2,6,7)]
    )
  }

DESeq2_pfun <-
  function(counts, group, design = NULL, cookcutoff, fittype, test, spikeins)
  {   
    if(!is.null(spikeins))
      counts <- rbind(spikeins, counts)
    colData <- data.frame(group)
    dse <- DESeqDataSetFromMatrix(countData = counts, colData = colData, design = ~ group)
    if(!is.null(spikeins))
      dse <- estimateSizeFactors(dse, controlGenes = 1:nrow(spikeins))
    else
      dse <- estimateSizeFactors(dse)
    dse <- estimateDispersions(dse, fitType = fittype)
    colData(dse)$group <- as.factor(colData(dse)$group)
    if(test == 'LRT')
      dse <- nbinomLRT(dse, reduced = ~1)
    else
      dse <- nbinomWaldTest(dse)
    if(cookcutoff == 'on')
      res <- results(dse)
    else
      res <- results(dse, cooksCutoff = FALSE)
    list(
      RawCount = counts,
      NormCount = counts(dse, normalized = TRUE),
      Dispersion = as.data.frame(mcols(dse)[,4:6]),
      TestStat = as.data.frame(res[, c(1,2,5)])
    )
  }

DESeq_pfun <-
  function(counts, group, disp_method, sharing_mode, fit_type, spikeins)
  {   
    de <- newCountDataSet(counts, group)
    if(!is.null(spikeins)){
      counts <- rbind(spikeins, counts)
      sizeFactors(de) <- DESeq2::estimateSizeFactorsForMatrix(counts, controlGenes = 1:nrow(spikeins))
    }
    else
      de <- estimateSizeFactors(de)
    de <- DESeq::estimateDispersions(de, method = disp_method, sharingMode = sharing_mode, fitType = fit_type)
    res <- nbinomTest(de, levels(group)[1], levels(group)[2])
    Disp <- DESeq::fitInfo(de)
    Dispersion <- data.frame(
      PerGeneEst <- Disp$perGeneDispEsts,
      FittedDispEst <- Disp$fittedDispEsts
    )
    colnames(Dispersion) <- c('PerGeneEst', 'FittedDispEst')
    list(
      RawCount = counts,
      NormCount = counts(de, normalized = TRUE),
      Dispersion = Dispersion,
      TestStat = as.data.frame(res[, c(2,6,7)])
    )
  }

edgeR.pfun <-
  function(counts, group, design = NULL, spikeins)
  {
    ## edgeR standard pipeline ##
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins, counts)
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W)
      rownames(Phenod) <- colnames(counts)
      design <- model.matrix(~x + W_1, data = Phenod)
    }
    d <- DGEList(counts = counts, group = group)
    d <- calcNormFactors(d)
    d <- estimateGLMCommonDisp(d, design = design)
    d <- estimateGLMTrendedDisp(d,design=design)
    d <- estimateGLMTagwiseDisp(d, design = design)
    f <- glmFit(d, design = design)
    lr <- glmLRT(f, coef=2)
    Dispersion <- data.frame(
      CommonDisp <- d$common.dispersion,
      TagwiseDisp <- d$tagwise.dispersion,
      FittedDisp <- d$trended.dispersion
    )
    colnames(Dispersion) <- c('CommonDisp', 'TagwiseDisp', 'FittedDisp')
    TestStat <- data.frame(
      AveCPM = 2^(d$AveLogCPM),
      logfc = lr$table$logFC,
      pval = lr$table$PValue
      )
    colnames(TestStat) <- c('AveCPM', 'logFC', 'p value')
    list(
      RawCount = counts,
      NormCount = cpm(d),
      Dispersion = Dispersion,
      TestStat = TestStat
        )
  }

edgeR_robust.pfun <-
  function(counts, group, design = NULL, spikeins)
  {   
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins, counts)
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W)
      rownames(Phenod) <- colnames(counts)
      design <- model.matrix(~x + W_1, data = Phenod)
    }
    d <- DGEList(counts = counts, group = group)
    d <- calcNormFactors(d)
    dw <- estimateGLMRobustDisp(d,design=design)
    fw <- glmFit(dw, design=design)
    lrw <- glmLRT(fw,coef=2)
    Dispersion <- data.frame(
      TagwiseDisp <- dw$tagwise.dispersion,
      FittedDisp <- dw$trended.dispersion
    )
    colnames(Dispersion) <- c('TagwiseDisp', 'FittedDisp')
    TestStat <- data.frame(
      AveCPM = 2^(dw$AveLogCPM),
      logfc = lr$table$logFC,
      pval = lr$table$PValue
    )
    colnames(TestStat) <- c('AveCPM', 'logFC', 'p value')
    list(
      RawCount = counts,
      NormCount = cpm(d),
      Dispersion = Dispersion,
      TestStat = TestStat
    )
  }


limma_voom.pfun <-
  function(counts, group, design = NULL, spikeins) 
  {   
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins, counts)
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W)
      rownames(Phenod) <- colnames(counts)
      design <- model.matrix(~x + W_1, data = Phenod)
    }
    nf <- calcNormFactors(counts)
    y <- voom(counts, design, plot=FALSE, lib.size = colSums(counts)*nf)
    fit <- lmFit(y, design)
    fit <- eBayes(fit)
    TestStat <- topTable(fit,coef=2,n=nrow(counts), sort.by = "none")
    TestStat <- data.frame(
      AveExpr = 2^(TestStat$AveExpr),
      logfc = TestStat$logFC,
      pval = TestStat$P.Value
    )
    colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
    return(list(
      RawCount = counts,
      NormCount = as.matrix(2^(y$E)),
      Dispersion = c(),
      TestStat = TestStat
    ))
  }

scde.pfun <- function(counts, design, cores = 4){
  err_mod <- scde.error.models(counts = counts, groups = design, n.cores = cores,
                             threshold.segmentation=T, save.crossfit.plots=F, 
                             save.model.plots=F,verbose=0)
  valid_cells <- err_mod$corr.a >0
  err_mod <- err_mod[valid_cells, ]
  counts <- counts[, valid_cells]
  design <- design[valid_cells]
  exprs_prior <- scde.expression.prior(models = err_mod,
                                       counts = counts,
                                       length.out=400,
                                       show.plot=F)
  names(design) <- row.names(err_mod)
  ediff <- scde.expression.difference(models = err_mod, counts = counts, 
                                      prior =  exprs_prior, groups = design,
                                      n.randomizations=100, n.cores = cores,verbose=1, 
                                      return.posteriors = F)
  norm_counts <- as.matrix(2^(scde.expression.magnitude(models = err_mod, counts = counts)))
  TestStat <- data.frame(
    AveExpr = apply(norm_counts, 1, mean),
    logfc = ediff$mle,
    pval = 2*pnorm(-abs(ediff$Z))
  )
  colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
  list(
    RawCount = counts,
    NormCount = norm_counts,
    Dispersion = c(),
    TestStat = TestStat
  )
}

Brennecke_pfun <- function(counts, spikeins){
  if(any(!is.integer(counts)))
    nCountsBio <- preprocessCore::normalize.quantiles(counts)
  else{
    sfBio <- estimateSizeFactorsForMatrix(counts)
    nCountsBio <- t(t(counts)/sfBio)
  }
  meansBio <- rowMeans( nCountsBio )
  varsBio <- rowVars( nCountsBio )
  cv2Bio <- varsBio / meansBio^2
  if(is.null(spikeins)){
    return(list(
      RawCount = counts,
      normCounts = nCountsBio,
      CV = cv2Bio,
      DE_id = order(cv2Bio, decreasing = T)[1:100]
    ))
  }
  if(any(!is.integer(counts)))
    nCountsTec <- preprocessCore::normalize.quantiles(spikeins)
  else{
    sfTec <- estimateSizeFactorsForMatrix(spikeins) 
    nCountsTec <- t(t(spikeins)/sfTec)
  }
  # Estimation of gene-specific expression rates
  meansTec <- rowMeans( nCountsTec )
  varsTec <- rowVars( nCountsTec )
  cv2Tec <- varsTec / meansTec^2

  
  # HVG detection
  minMeanForFit <- unname( quantile( meansTec[ which( cv2Tec > .3 ) ], .95 ) )
  useForFit <- meansTec >= minMeanForFit
  fit <- glmgam.fit( cbind( a0 = 1, a1tilde = 1/meansTec[useForFit] ),cv2Tec[useForFit] )
  xi <- mean( 1 / sfTec )
  a0 <- unname( fit$coefficients["a0"] )
  a1 <- unname( fit$coefficients["a1tilde"] - xi )
  psia1theta <- mean( 1 / sfBio ) + a1 * mean( sfTec / sfBio )
  minBiolDisp <- .5^2
  m <- ncol(counts)
  cv2th <- a0 + minBiolDisp + a0 * minBiolDisp
  testDenom <- ( meansBio * psia1theta + meansBio^2 * cv2th ) / ( 1 + cv2th/m )
  p <- 1 - pchisq( varsBio * (m-1) / testDenom, m-1 )
  padj <- p.adjust( p, "BH" )
  sig <- padj < .1
  sig[is.na(sig)] <- FALSE
  HVG_Bren_id=which(sig)
  return(list(
    RowCount = counts,
    normCounts = nCountsBio,
    CV = cv2Bio,
    DE_id = HVG_Bren_id
  ))
}

limma.pfun <- function(counts, group, design){
  counts_N <- preprocessCore::normalize.quantiles(counts)
  counts_N_log2 <- log2(counts_N)
  fit <- lmFit(counts_N_log2, design)
  contrast.matrix <- makeContrasts(levels(design)[1] - levels(design)[2],
                                     levels=design)
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit2 <- eBayes(fit2)
  TestStat <- topTable(fit2,coef=2,n=nrow(counts), sort.by = "none")
  TestStat <- data.frame(
    AveExpr = 2^(TestStat$AveExpr),
    logfc = TestStat$logFC,
    pval = TestStat$P.Value
  )
  list(
    RawCount = counts,
    NormCount = counts_N,
    Dispersion = c(),
    TestStat = TestStat
  )
}

monocle.pfun <- function(counts, group){
  counts_N <- preprocessCore::normalize.quantiles(counts)
  temp <- t(counts_N)
  temp$condition <- group
  temp %>% group_by(condition) %>% summarize_each(funs = funs(mean))
  design <- data.frame(conditions = group)
  rownames(design) <- colnames(counts)
  pd <- new("AnnotatedDataFrame", data = design)
  counts_class <- newCellDataSet(as.matrix(counts_N), phenoData = pd)
  diff_test_res <- differentialGeneTest(counts_class,
                                        fullModelFormulaStr="expression~conditions")
  TestStat <- data.frame(
    AveExpr = apply(counts_N, 1, mean),
    logfc = log2(temp[1,]/temp[2,]),
    pval = diff_test_res$pval
  )
  list(
    RawCount = counts,
    NormCount = counts_N,
    Dispersion = c(),
    TestStat = TestStat
  )
}

SAMSeq.pfun <- function(counts, group){
  TestStat <- SAMseq(as.matrix(counts), group, resp.type = "Two class unpaired")
  TestStat <- data.frame(
    AveExpr = 2^(TestStat$AveExpr),
    logfc = TestStat$logFC,
    pval = TestStat$P.Value
  )
  list(
    RawCount = counts,
    NormCount = counts_N,
    Dispersion = c(),
    TestStat = TestStat
  )
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



