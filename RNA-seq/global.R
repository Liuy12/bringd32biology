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
library(BPSC)
library(RUVSeq)
library(statmod)
library(monocle)
library(igraph)
library(NetSAM)
library(LPCM)
library(colorspace)
library(doParallel)
library(EBSeq)
library(ROTS)


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

XBSeq.pfun <- 
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

DESeq2.pfun <-
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

DESeq.pfun <-
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
      #rownames(Phenod) <- colnames(counts)[sample_sel]
      design <- model.matrix(~x + weight, data = Phenod)
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
    rownames(TestStat) <- rownames(counts)
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
      #rownames(Phenod) <- colnames(counts)[sample_sel]
      design <- model.matrix(~x + weight, data = Phenod)
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
    rownames(TestStat) <- rownames(counts)
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
    nf <- calcNormFactors(counts)
    y <- voom(counts, design, plot=FALSE, lib.size = colSums(counts)*nf)
    NormCount <- as.matrix(2^(y$E))
    if(!is.null(condition_sel))
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
    else
      sample_sel <- 1:ncol(counts)
    group <- group[sample_sel]
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins[,sample_sel], counts[,sample_sel])
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W[sample_sel])
      #rownames(Phenod) <- colnames(counts)[sample_sel]
      design <- model.matrix(~x + weight, data = Phenod)
    }
    else
      design <- model.matrix(~group)
    nf <- calcNormFactors(counts[,sample_sel])
    y <- voom(counts[,sample_sel], design, plot=FALSE, lib.size = colSums(counts)*nf)
    fit <- lmFit(y, design)
    fit <- eBayes(fit)
    TestStat <- topTable(fit,coef=2,n=nrow(counts), sort.by = "none")
    TestStat <- data.frame(
      AveExpr = 2^(TestStat$AveExpr),
      logfc = TestStat$logFC,
      pval = TestStat$P.Value
    )
    colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
    rownames(TestStat) <- rownames(counts)
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
  rownames(TestStat) <- rownames(counts)
  colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
  list(
    RawCount = counts,
    NormCount = norm_counts,
    Dispersion = c(),
    TestStat = TestStat
  )
}

Brennecke.pfun <- function(counts, spikeins, nums){
  if(any(round(counts) != counts)){
    nCountsBio <- preprocessCore::normalize.quantiles(as.matrix(counts))
    rownames(nCountsBio) <- rownames(counts)
    colnames(nCountsBio) <- colnames(counts)
  }
  else{
    sfBio <- estimateSizeFactorsForMatrix(counts)
    nCountsBio <- t(t(counts)/sfBio)
  }
  nCountsBio <- nCountsBio
  meansBio <- rowMeans( nCountsBio )
  varsBio <- apply( nCountsBio, 1, var )
  cv2Bio <- varsBio / meansBio^2
  if(is.null(spikeins)){
    return(list(
      RawCount = counts,
      normCounts = nCountsBio,
      CV = cv2Bio,
      DE_id = order(cv2Bio, decreasing = T)[1:nums]
    ))
  }
  if(any(round(spikeins) != spikeins))
    nCountsTec <- preprocessCore::normalize.quantiles(as.matrix(spikeins))
  else{
    sfTec <- estimateSizeFactorsForMatrix(spikeins) 
    nCountsTec <- t(t(spikeins)/sfTec)
  }
  # Estimation of gene-specific expression rates
  meansTec <- rowMeans( nCountsTec )
  varsTec <- apply( nCountsTec, 1, var )
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

limma.pfun <- function(counts, group, design, spikeins, condition_sel){
  counts_N <- preprocessCore::normalize.quantiles(as.matrix(counts))
  rownames(counts_N) <- rownames(counts)
  colnames(counts_N) <- colnames(counts)
  counts_N_log2 <- log2(counts_N + 0.01)
  if(!is.null(condition_sel))
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
  else
    sample_sel <- 1:ncol(counts)
  group <- group[sample_sel]
    if(!is.null(spikeins)){
      counts1 <- rbind(spikeins, counts)
      set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
      Phenod <- data.frame(x = group, weight = set$W[sample_sel])
      #rownames(Phenod) <- colnames(counts)
      design <- model.matrix(~x + weight, data = Phenod)
    }
    else
      design <- model.matrix(~group)
    counts_N_log2 <- counts_N_log2[,sample_sel]
  fit <- lmFit(counts_N_log2, design)
  fit <- eBayes(fit)
  TestStat <- topTable(fit,coef=2,n=nrow(counts), sort.by = "none")
  TestStat <- data.frame(
    AveExpr = 2^(TestStat$AveExpr),
    logfc = TestStat$logFC,
    pval = TestStat$P.Value
  )
  rownames(TestStat) <- rownames(counts)
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
  rownames(TestStat) <- rownames(counts)
  list(
    RawCount = counts,
    NormCount = counts_N,
    Dispersion = c(),
    TestStat = TestStat
  )
}

BPSC.pfun <- function(counts, group, design, spikeins, condition_sel, cores = 2){
  # the input of BPSC has to be TPM or 
  # normlized counts from edgeR
  d <- DGEList(counts = counts, group = group)
  d <- calcNormFactors(d)
  NormCount <- cpm(d)
  if(!is.null(condition_sel))
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
  else
    sample_sel <- 1:ncol(counts)
  group <- group[sample_sel]
  if(!is.null(spikeins)){
    counts1 <- rbind(spikeins, counts)
    set <- RUVg(as.matrix(counts1), 1:nrow(spikeins), k=1)
    Phenod <- data.frame(x = group, weight = set$W[sample_sel])
    #rownames(Phenod) <- colnames(counts)
    design <- model.matrix(~x + weight, data = Phenod)
  }
  else
    design <- model.matrix(~group)
  #Run BPglm for differential expression analysis
  registerDoParallel(cores=cores)
  res <- BPglm(data=NormCount[,sample_sel], controlIds=which(group==unique(group)[1]), design=design, coef=2, estIntPar=FALSE, useParallel = T) 
  temp <- data.frame(
    c1 = apply(NormCount[,sample_sel][, group == unique(group)[1]], 1, mean),
    c2 = apply(NormCount[,sample_sel][, group == unique(group)[2]], 1, mean)
  )
  TestStat <- data.frame(
    AveExpr = apply(NormCount[,sample_sel], 1, mean),
    logfc = log2(temp[1,]/temp[2,]),
    pval = res$PVAL
  )
  rownames(TestStat) <- rownames(counts)
  colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
  list(
    RawCount = counts,
    NormCount = NormCount,
    Dispersion = c(),
    TestStat = TestStat
  )
}

MAST.pfun <- function(counts, group, spikeins, condition_sel){
  d <- DGEList(counts = counts, group = group)
  d <- calcNormFactors(d)
  NormCount <- cpm(d)
  if(!is.null(condition_sel))
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
  else
    sample_sel <- 1:ncol(counts)
  cat('checkpoint4', '\n')
  cat('condition:', str(condition_sel), '\n')
  str(sample_sel)
  str(counts)
  str(as.matrix(log2(counts[,sample_sel] + 1)))
  scAssay <- FromMatrix(as.matrix(log2(counts[,sample_sel] + 1)), cData = data.frame(condition = group[sample_sel], wellKey = colnames(counts)[sample_sel]), fData = data.frame(primerid = rownames(counts)))
  zlmCond <- zlm.SingleCellAssay(~condition, scAssay)
  cat('checkpoint6', '\n')
  contrast <- colnames(zlmCond@coefD)[2]
  summaryLrt <- summary(zlmCond, doLRT=contrast)
  summaryDt <- summaryLrt$datatable
  fcHurdle <- merge(summaryDt[contrast==contrast & component=='H',.(primerid, `Pr(>Chisq)`)],
                    summaryDt[contrast==contrast & component=='logFC', .(primerid, coef, ci.hi, ci.lo)], by='primerid')
  TestStat <- data.frame(
    AveExpr = apply(NormCount[,sample_sel], 1, mean),
    logfc = fcHurdle$coef,
    pval = fcHurdle$`Pr(>Chisq)`
  )
  cat('checkpoint7', '\n')
  rownames(TestStat) <- rownames(counts)
  colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
  list(
    RawCount = counts,
    NormCount = NormCount,
    Dispersion = c(),
    TestStat = TestStat
  )
}

EBSeq.pfun <- 
  function(counts, group, condition_sel)
  {
    ## EBSeq pipeline ##
    ## adjust p value and p value are the same
    sf <- MedianNorm(counts)
    NormCount <- t(t(counts)/sf)
    if(!is.null(condition_sel))
      sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
    else
      sample_sel <- 1:ncol(counts)
    group <- as.factor(group[sample_sel])
    f <- EBTest(Data = counts[,sample_sel], Conditions = group, sizeFactors = sf[sample_sel], maxround = 5)
    res <- GetDEResults(f)
    TestStat <- data.frame(
      AveExpr = apply(NormCount[,sample_sel], 1, mean),
      logfc = log2(PostFC(f)$PostFC),
      pval = 1 - res$PPMat[, "PPDE"]
    )
    rownames(TestStat) <- rownames(counts)
    colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
    list(
      RawCount = counts,
      NormCount = f$DataNorm,
      Dispersion = c(),
      TestStat = TestStat
    )
  }	

ROTS.pfun <- function(counts, group, condition_sel, B =1000){
  # the input of ROTS has to be 
  # normlized counts or TPM from edgeR
  d <- DGEList(counts = counts, group = group)
  d <- calcNormFactors(d)
  NormCount <- cpm(d)
  if(!is.null(condition_sel))
    sample_sel <- c(grep(condition_sel[1], group), grep(condition_sel[2], group))
  else
    sample_sel <- 1:ncol(counts)
  group <- group[sample_sel]
  res <- ROTS(data = NormCount[,sample_sel], groups = group, B = B, K = 500 , seed = 1234, log = FALSE)
  TestStat <- data.frame(
    AveExpr = apply(NormCount[,sample_sel], 1, mean),
    logfc = res$logfc,
    pval = res$pvalue
  )
  rownames(TestStat) <- rownames(counts)
  colnames(TestStat) <- c('AveExpr',  'logFC', 'p value')
  list(
    RawCount = counts,
    NormCount = NormCount,
    Dispersion = c(),
    TestStat = TestStat
  )
}


idHetero <-
  function(inputNetwork, minMod = 5, maxStep = 5, permuteNum = 1000, pThr = 0.05, weight = NULL) {
    inputNetwork <- as.matrix(inputNetwork)
    inputNetwork_S <- as.character(inputNetwork)
    inputNetwork_S <- array(inputNetwork_S,dim = dim(inputNetwork))
    network <- graph.edgelist(inputNetwork_S,directed = F)
    rm(inputNetwork,inputNetwork_S)
    cellInNetwork1 <- V(network)$name
    if(is.null(weight))
      weight <- rep(1, length(E(network)))
    E(network)$weight <- weight
    cat("Network has ",vcount(network)," nodes and ",ecount(network)," edges\n",sep =
          "")
    
    # simplify the edge by removing loops and multiple edges
    network <- simplify(network)
    cellInNetwork <- V(network)$name
    if (length(cellInNetwork1) != length(cellInNetwork)) {
      cat(
        "After removing self interactions and loop interactions, network remains ",vcount(network)," nodes and ",ecount(network)," edges\n",sep =
          ""
      )
    }
    
    minModule <-
      ifelse(round(length(cellInNetwork) * (0.003)) > minMod, round(length(cellInNetwork) *
                                                                      (0.003)), minMod)
    
    overlap_cell_networkP <- V(network)$name
    subnetworkInfo <- list()
    subnetworkInfo_index <- 1
    ## calculate the maximal connected components of a graph
    network_cluster <- components(network)
    network_cluster_size <- network_cluster$csize
    
    if (length(network_cluster_size[network_cluster_size >=
                                    minModule]) == 0) {
      stop(
        "The size of all subnetworks in the inputted network are less than ",minModule,". Please adjust the parameter 'minModule'!\n\n"
      )
      
    }
    
    if (length(network_cluster_size) >= 2 &
        sort(network_cluster_size, decreasing = T)[2] > minModule)
      return(list(sig = 1, pval = NA))
    
    network_cluster_size <-
      data.frame(
        id = c(1:length(network_cluster_size)),cluster_size = network_cluster_size,stringsAsFactors =
          F
      )
    network_cluster_size <-
      network_cluster_size[order(network_cluster_size[,2], decreasing = T),]
    
    network_cluster_membership <-
      network_cluster$membership
    
    subnetwork_id <- 1
    for (i in c(1:nrow(network_cluster_size))) {
      sub_network_size <- network_cluster_size[i,2]
      if (sub_network_size >= minModule) {
        cat("Start to analysis subnetwork ",subnetwork_id,"!\n")
        subnetwork_id <- subnetwork_id + 1
        subnetwork_node <-
          overlap_cell_networkP[which(network_cluster_membership == network_cluster_size[i,1])]
        subnetwork <-
          induced.subgraph(network,subnetwork_node)
        network_info <-
          evaluateWalktrapStep(subnetwork,maxStep,level = 1)
        if(length(unique(network_info$walktrap$membership)) == 1){
          sig <- 0
          pval <- NA
        }
        else{
          network_sig <-identifySig(network_info,permuteNum, pThr, weight = weight)
          sig <- network_sig$sig
          pval <- network_sig$pval
        }
        return(list(sig = sig, pval = pval, networkInfo = network_info))
      }
    }
  }


evaluateWalktrapStep <- function(network_igraph,maxStep,level) {
  #evaluate the optimal Step for the network
  
  network_info <- list()
  network_walktrap <- walktrap.community(network_igraph,steps = 2)
  modularityMax <- max(network_walktrap$modularity)
  optimalwalktrap <- network_walktrap
  optimalStep <- 2
  
  for (i in c(3:maxStep)) {
    network_walktrap <- walktrap.community(network_igraph, weights =  E(network_igraph)$weight, steps = i)
    network_modularity <- max(network_walktrap$modularity)
    #cat("Modularity:",network_modularity,"\n")
    if (network_modularity > modularityMax) {
      optimalwalktrap <- network_walktrap
      optimalStep <- i
      modularityMax <- network_modularity
    }
  }
  maxWalktrap <-
    list(
      walktrap = optimalwalktrap,step = optimalStep,network = network_igraph,level =
        level
    )
  return(maxWalktrap)
}



identifySig <-
  function(network_info,permuteNum, pThr, weight) {
    #identify whether the network can be separated again
    
    network_walktrap <- network_info$walktrap
    network_modularity <- max(network_walktrap$modularity)
    network_igraph <- network_info$network
    degree <- igraph:::degree(network_igraph)
    ranmodu <- vector()
    step <- network_info$step
    sig <- 0
    for (i in c(1:permuteNum)) {
      cat('The ', i, 'th ' ,'time', '\n')
      suppressWarnings(rannet <-
                         degree.sequence.game(degree,method = "vl"))
      ran_walktrap <- walktrap.community(rannet, weights = weight, steps = step)
      ranModularity <- max(ran_walktrap$modularity)
      ranmodu <- c(ranmodu,ranModularity)
    }
    
    p <- length(ranmodu[ranmodu >= network_modularity]) / permuteNum
    if (p < pThr) {
      sig <- 1
    }
    return(list(sig = sig, pval = p))
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

