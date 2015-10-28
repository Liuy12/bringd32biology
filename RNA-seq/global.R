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
# library(samr) not easy to implement 
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
  function(counts, bgcounts, group, disp_method, sharing_mode, fit_type, paraMethod, spikeins, condition_sel){
    if(!is.null(condition_sel)){
      XB <- XBSeqDataSet(counts, bgcounts, group)
      XB <- estimateRealCount(XB)
      if(!is.null(spikeins)){
        counts <- rbind(spikeins, counts(XB))
        sizeFactors(XB) <- DESeq2::estimateSizeFactorsForMatrix(counts, controlGenes = 1:nrow(spikeins))
      }
      else
        XB <- estimateSizeFactors(XB)
      NormCount <- counts(XB, normalized = TRUE)
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
    }
    else{
      sample_sel <- 1:ncol(counts)
      NormCount <- c()
    }
    XB <- XBSeqDataSet(counts[,sample_sel], bgcounts[,sample_sel], group[sample_sel])
    XB <- estimateRealCount(XB)
    if(!is.null(spikeins)){
      counts <- rbind(spikeins[,sample_sel], counts(XB))
      sizeFactors(XB) <- DESeq2::estimateSizeFactorsForMatrix(counts[,sample_sel], controlGenes = 1:nrow(spikeins[,sample_sel]))
    }
    else
      XB <- estimateSizeFactors(XB)
    if(is.null(NormCount))
      NormCount <- counts(XB, normalized = TRUE)
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
      NormCount = NormCount,
      Dispersion = Dispersion,
      TestStat = Teststas[,c(2,6,7)]
    )
    }

DESeq2_pfun <-
  function(counts, group, design = NULL, cookcutoff, fittype, test, spikeins, condition_sel)
  {   
    if(!is.null(spikeins))
      counts <- rbind(spikeins, counts)
    if(!is.null(condition_sel)){
      if(!is.null(spikeins))
        sf <- estimateSizeFactorsForMatrix(counts, controlGenes = 1:nrow(spikeins))
      else
        sf <- estimateSizeFactorsForMatrix(counts)
      NormCount <- t(t(counts)/sf)
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
    }
    else{
      NormCount <- c()
      sample_sel <- 1:ncol(counts)
    }
    colData <- data.frame(group[sample_sel])
    colnames(colData) <- 'group'
    dse <- DESeqDataSetFromMatrix(countData = counts[,sample_sel], colData = colData, design = ~ group)
    if(!is.null(spikeins))
      dse <- estimateSizeFactors(dse, controlGenes = 1:nrow(spikeins))
    else
      dse <- estimateSizeFactors(dse)
    if(is.null(NormCount))
      NormCount <- counts(dse, normalized = TRUE)
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
      NormCount = NormCount,
      Dispersion = as.data.frame(mcols(dse)[,4:6]),
      TestStat = as.data.frame(res[, c(1,2,5)])
    )
  }

DESeq_pfun <-
  function(counts, group, disp_method, sharing_mode, fit_type, spikeins)
  {   
    de <- newCountDataSet(counts, group)
    if(!is.null(condition_sel)){
      if(!is.null(spikeins)){
        counts <- rbind(spikeins, counts)
        sf <- estimateSizeFactorsForMatrix(counts, controlGenes = 1:nrow(spikeins))
      }
      else
        sf <- estimateSizeFactorsForMatrix(counts)
      NormCount <- t(t(counts)/sf)
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
      }
    else{
      NormCount <- c()
      sample_sel <- 1:ncol(counts)
    }
    if(!is.null(spikeins)){
      counts <- rbind(spikeins[,sample_sel], counts[,sample_sel])
      sizeFactors(de) <- DESeq2::estimateSizeFactorsForMatrix(counts[,sample_sel], controlGenes = 1:nrow(spikeins))
    }
    else
      de <- estimateSizeFactors(de)
    if(is.null(NormCount))
      NormCount <- counts(de, normalized = TRUE)
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
      NormCount = NormCount,
      Dispersion = Dispersion,
      TestStat = as.data.frame(res[, c(2,6,7)])
    )
  }

edgeR.pfun <-
  function(counts, group, design = NULL, spikeins, condition_sel)
  {
    if(!is.null(condition_sel)){
      d <- DGEList(counts = counts, group = group)
      d <- calcNormFactors(d)
      NormCount <- cpm(d)
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
      group <- group[sample_sel]
      design <- model.matrix(~group)
       }
    else{
      NormCount <- c()
      sample_sel <- 1:ncol(counts)
    }
    ## edgeR standard pipeline ##
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins[,sample_sel], counts[,sample_sel])
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W)
      rownames(Phenod) <- colnames(counts)[sample_sel]
      design <- model.matrix(~x + W_1, data = Phenod)
    }
    d <- DGEList(counts = counts[,sample_sel], group = group)
    d <- calcNormFactors(d)
    if(is.null(NormCount))
      NormCount <- cpm(d)
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
      NormCount = NormCount,
      Dispersion = Dispersion,
      TestStat = TestStat
        )
  }

edgeR_robust.pfun <-
  function(counts, group, design = NULL, spikeins, condition_sel)
  {  
    if(!is.null(condition_sel)){
      d <- DGEList(counts = counts, group = group)
      d <- calcNormFactors(d)
      NormCount <- cpm(d)
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
      group <- group[sample_sel]
      design <- model.matrix(~group)
      }
    else{
      NormCount <- c()
      sample_sel <- 1:ncol(counts)
    }
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins[, sample_sel], counts[,sample_sel])
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W)
      rownames(Phenod) <- colnames(counts)[sample_sel]
      design <- model.matrix(~x + W_1, data = Phenod)
    }
    d <- DGEList(counts = counts[,sample_sel], group = group)
    d <- calcNormFactors(d)
    if(is.null(NormCount))
      NormCount <- cpm(d)
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
      NormCount = NormCount,
      Dispersion = Dispersion,
      TestStat = TestStat
    )
  }


limma_voom.pfun <-
  function(counts, group, design = NULL, spikeins, condition_sel) 
  {   
    if(!is.null(condition_sel)){
      if(!is.null(spikeins)){
        counts1 <- rbind(spikeins, counts)
        set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
        Phenod <- data.frame(x = group, weight = set$W)
        rownames(Phenod) <- colnames(counts)
        design <- model.matrix(~x + W_1, data = Phenod)
      }
      nf <- calcNormFactors(counts)
      y <- voom(counts, design, plot=FALSE, lib.size = colSums(counts)*nf)
      NormCount <- as.matrix(2^(y$E))
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
      group <- group[sample_sel]
      design <- model.matrix(~group)
    }
    else{
      NormCount <- c()
      sample_sel <- 1:ncol(counts)
    }
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins[,sample_sel], counts[,sample_sel])
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W)
      rownames(Phenod) <- colnames(counts)[sample_sel]
      design <- model.matrix(~x + W_1, data = Phenod)
    }
    nf <- calcNormFactors(counts[,sample_sel])
    y <- voom(counts[,sample_sel], design, plot=FALSE, lib.size = colSums(counts)*nf)
    if(is.null(NormCount))
      NormCount <- as.matrix(2^(y$E))
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
      NormCount = NormCount,
      Dispersion = c(),
      TestStat = TestStat
    ))
  }

scde.pfun <- function(counts, design, cores = 4, condition_sel){
  if(condition_sel[1]!=""){
    err_mod <- scde.error.models(counts = counts, groups = design, n.cores = cores,
                                 threshold.segmentation=T, save.crossfit.plots=F, 
                                 save.model.plots=F,verbose=0, min.size.entries = 100)
    valid_cells <- err_mod$corr.a >0
    err_mod <- err_mod[valid_cells, ]
    counts <- counts[, valid_cells]
    norm_counts <- as.matrix(2^(scde.expression.magnitude(models = err_mod, counts = counts)))
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
  }
  else {
    norm_counts <- c()
    sample_sel <- 1:ncol(counts)
  }
  err_mod <- scde.error.models(counts = counts[,sample_sel], groups = design[sample_sel], n.cores = cores,
                             threshold.segmentation=T, save.crossfit.plots=F, 
                             save.model.plots=F,verbose=0, min.size.entries = 90)
  valid_cells <- err_mod$corr.a >0
  err_mod <- err_mod[valid_cells, ]
  counts <- counts[, sample_sel[valid_cells]]
  design <- design[sample_sel[valid_cells]]
  exprs_prior <- scde.expression.prior(models = err_mod,
                                       counts = counts,
                                       length.out=400,
                                       show.plot=F)
  names(design) <- row.names(err_mod)
  design <- as.factor(design)
  ediff <- scde.expression.difference(models = err_mod, counts = counts, 
                                      prior =  exprs_prior, groups = design,
                                      n.randomizations=100, n.cores = cores,verbose=1, 
                                      return.posteriors = F)
  if(is.null(norm_counts))
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

limma.pfun <- function(counts, group, design, condition_sel){
  counts_N <- preprocessCore::normalize.quantiles(counts)
  counts_N_log2 <- log2(counts_N)
  if(!is.null(condition_sel)){
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
    group <- group[sample_sel]
    design <- model.matrix(~group)
    counts_N_log2 <- counts_N_log2[,sample_sel]
  }
  fit <- lmFit(counts_N_log2, design)
  fit <- eBayes(fit)
  TestStat <- topTable(fit,coef=2,n=nrow(counts), sort.by = "none")
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

monocle.pfun <- function(counts, group, condition_sel){
  counts_N <- as.data.frame(preprocessCore::normalize.quantiles(as.matrix(counts)))
  rownames(counts_N) <- rownames(counts)
  colnames(counts_N) <- colnames(counts)
  if(!is.null(condition_sel)){
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
    group <- group[sample_sel]
    counts_N_sel <- counts_N[,sample_sel]
  }
  else
    counts_N_sel <- counts_N
  temp <- data.frame(
    c1 = apply(counts_N_sel[, group == unique(group)[1]], 1, mean),
    c2 = apply(counts_N_sel[, group == unique(group)[2]], 1, mean)
  )
  design <- data.frame(conditions = group)
  rownames(design) <- colnames(counts)
  pd <- new("AnnotatedDataFrame", data = design)
  counts_class <- newCellDataSet(as.matrix(counts_N_sel), phenoData = pd)
  diff_test_res <- differentialGeneTest(counts_class,
                                        fullModelFormulaStr="expression~conditions", cores = 4)
  TestStat <- data.frame(
    AveExpr = apply(counts_N_sel, 1, mean),
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

# SAMSeq.pfun <- function(counts, group, condition_sel){
#   TestStat <- SAMseq(as.matrix(counts), as.factor(group), resp.type = "Two class unpaired")
#   TestStat <- data.frame(
#     AveExpr = 2^(TestStat$AveExpr),
#     logfc = TestStat$logFC,
#     pval = TestStat$P.Value
#   )
#   list(
#     RawCount = counts,
#     NormCount = counts_N,
#     Dispersion = c(),
#     TestStat = TestStat
#   )
# }


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

# pandoc_convert <- function(input,
#                            to = NULL,
#                            from = NULL,
#                            output = NULL,
#                            citeproc = FALSE,
#                            options = NULL,
#                            verbose = FALSE,
#                            wd = NULL) {
#   
#   # ensure we've scanned for pandoc
#   find_pandoc()
#   
#   # execute in specified working directory
#   if (is.null(wd)) {
#     wd <- base_dir(input)
#   }
#   oldwd <- setwd(wd)
#   on.exit(setwd(oldwd), add = TRUE)
#   
#   
#   # input file and formats
#   args <- c(input)
#   if (!is.null(to))
#     args <- c(args, "--to", to)
#   if (!is.null(from))
#     args <- c(args, "--from", from)
#   
#   #  output file
#   if (!is.null(output))
#     args <- c(args, "--output", output)
#   
#   # additional command line options
#   args <- c(args, options)
#   
#   # set pandoc stack size
#   stack_size <- getOption("pandoc.stack.size", default = "1024m")
#   args <- c(c("+RTS", paste0("-K", stack_size), "-RTS"), args)
#   
#   # build the conversion command
#   command <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "))
#   
#   # show it in verbose mode
#   if (verbose)
#     cat(command, "\n")
#   
#   # run the conversion
#   with_pandoc_safe_environment({
#     result <- system(command)
#   })
#   if (result != 0)
#     stop("pandoc document conversion failed with error ", result, call. = FALSE)
#   
#   invisible(NULL)
# }

