#library(googleAuthR)
library(shiny)
library(shinydashboard)
library(magrittr)
library(rCharts)
library(shinythemes)
library(RUVSeq)
library(colorspace)
library(doParallel)
library(slidify)
library(slidifyLibraries)
library(ggplot2)
### the following packages will be loaded within corresponding functions
### to avoid maximal number of DLLs reached error
#library(scatterplot3d)
#library(GGally)
#library(data.table)
#library(dplyr)
#library(threejs)
#library(d3heatmap)
#library(metricsgraphics)
#library(networkD3)
#library(DT)
# library(DESeq)
# library(DESeq2)
# library(XBSeq)
# library(edgeR)
# library(limma)
# library(scde)
# library(BPSC)
# library(monocle)
# library(statmod)
# library(igraph)
# library(NetSAM)
# library(LPCM)
# library(EBSeq)


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
    require(XBSeq)
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
    require(DESeq2)
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
    Dispersions <- as.data.frame(mcols(dse)[,4:6])
    list(
      RawCount = counts,
      NormCount = NormCount,
      Dispersion = Dispersions,
      TestStat = as.data.frame(res[, c(1,2,5)])
    )
  }

DESeq.pfun <-
  function(counts, group, disp_method, sharing_mode, fit_type, spikeins)
  {   
    require(DESeq)
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
    require(edgeR)
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
    require(edgeR)
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
      logfc = lrw$table$logFC,
      pval = lrw$table$PValue
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
    require(limma)
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

scde.pfun <- function(counts, design, cores = 10, condition_sel){
  require(scde)
  counts <- clean.counts(counts, min.lib.size=1000, min.reads = 1, min.detected = 1)
  if(!is.null(condition_sel)){
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

Brennecke.pfun <- function(counts, spikeins, nums, padj, pcut){
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
  padj <- p.adjust( p, padj)
  sig <- padj < pcut
  sig[is.na(sig)] <- FALSE
  HVG_Bren_id=which(sig)
  return(list(
    RowCount = counts,
    normCounts = nCountsBio,
    CV = cv2Bio,
    DE_id = HVG_Bren_id
  ))
}

scVEGs.pfun <- function(data, padj, pcut) {
  require(locfit)
  require(MASS)
  require(hydroGOF)
  require(calibrate)
  
  if(any(round(data) != data)){
    nCountsBio <- preprocessCore::normalize.quantiles(as.matrix(data))
    rownames(nCountsBio) <- rownames(data)
    colnames(nCountsBio) <- colnames(data)
  }
  else{
    sfBio <- estimateSizeFactorsForMatrix(data)
    nCountsBio <- t(t(data)/sfBio)
  }
  
  geneName <- rownames(data)

  m <- dim(data)[1]
  std <- apply(data, 1, sd)
  avg <- apply(data, 1, mean)
  cv <- std / avg
  # over dispersion sigma  (var = u(1 + u * sigma^2))
  rm(tpm)
  rm(flag)
  xdata <- (avg)
  ydata <- log10(cv)
  xdata <- xdata[is.na(ydata) != "TRUE"]
  ydata <- ydata[is.na(ydata) != "TRUE"]
  
  fitLoc <- locfit.robust(ydata ~ lp(log10(xdata), nn = .2))
  xSeq <- seq(min(log10(xdata)), max(log10(xdata)), 0.005)
  gapNum <- matrix(0, length(xSeq), 1)
  for(i in 1:length(xSeq)) {
    cdx <- which((log10(xdata) >= xSeq[i] - 0.05) & (log10(xdata) < xSeq[i] + 0.05))
    gapNum[i,1] <- length(cdx)
  }
  cdx <- which(gapNum > m*0.005)
  xSeq <- 10 ^ xSeq
  ySeq <- predict(fitLoc,log10(xSeq))
  yDiff <- diff(ySeq)
  ix <- which(yDiff > 0 & log10(xSeq[-1]) > 0)
  if(length(ix) == 0)
    ix <- length(ySeq) - 1
  xSeq_all <- 10^seq(min(log10(xdata)), max(log10(xdata)), 0.001)
  xSeq <- xSeq[cdx[1]:ix[1] + 1]
  ySeq <- ySeq[cdx[1]:ix[1] + 1]
  
  b <- 1
  a <- 0
  df <- data.frame(x=xSeq, y = ySeq)
  fit = nls(y ~ 0.5 * log10(b / x + a), data = df, 
            start=list(b = b,a = a), nls.control(maxiter = 500), na.action =  'na.exclude')
  newdf <- data.frame(x = xSeq_all)
  ydataFit <- predict(fit,newdata = newdf)
  
  # Calculate CV difference
  logX <- log10(xdata)
  
  logXseq <- log10(xSeq_all)
  cvDist <- matrix(0,length(xdata),1)
  for (i in 1:length(logX)) {
    cx <- which(logXseq >= logX[i] - 0.2 & logXseq < logX[i] + 0.2)
    tmp <- sqrt((logXseq[cx] - logX[i])^2 + (ydataFit[cx] - ydata[i])^2)
    tx <- which.min(tmp)
    
    if(logXseq[cx[tx]] > logX[i]) {
      if(ydataFit[cx[tx]] > ydata[i]) {
        cvDist[i] <- -1*tmp[tx]
      } else {
        cvDist[i] <- tmp[tx]
      }
      cvDist[i] <- -1*tmp[tx]
    } else if (logXseq[cx[tx]] <= logX[i]) {
      if(ydataFit[cx[tx]] < ydata[i]) {
        cvDist[i] <- tmp[tx]
      } else {
        cvDist[i] <- -1*tmp[tx]
      }
    } 
  }
  cvDist <- log2(10^cvDist)
  
  # use kernel density estimate to find the peak
  dor <- density(cvDist, kernel = "gaussian")
  distMid <-dor$x[which.max(dor$y)]
  dist2 <- cvDist - distMid
  tmpDist <- c(dist2[dist2 <= 0], abs(dist2[dist2 < 0])) + distMid
  distFit <- fitdistr(tmpDist, "normal")
  pRaw <- pnorm(cvDist, mean = distFit$estimate[1], sd = distFit$estimate[2], lower.tail = FALSE)
  pAdj <- p.adjust(pRaw, padj)
  dx <- which(pAdj < pVal)
  
  sig <- data[dx, ]
  return(list(
    RowCount = data,
    normCounts = nCountsBio,
    CV = cv,
    DE_id = dx
  ))
  }

limma.pfun <- function(counts, group, design, spikeins, condition_sel){
  require(limma)
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
  require(monocle)
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
                                        fullModelFormulaStr="expression~conditions", cores = 10)
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

BPSC.pfun <- function(counts, group, design, spikeins, condition_sel, cores = 10){
  # the input of BPSC has to be TPM or 
  # normlized counts from edgeR
  require(BPSC)
  require(edgeR)
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
    logfc = log2(temp[,1]/temp[,2]),
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
  require(MAST)
  require(edgeR)
  registerDoParallel(cores=10)
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
  scAssay <- FromMatrix('SingleCellAssay', t(as.matrix(log2(counts[,sample_sel] + 1))), cData = data.frame(condition = group[sample_sel], wellKey = colnames(counts)[sample_sel]), fData = data.frame(primerid = rownames(counts)))
  zlmCond <- zlm.SingleCellAssay(~condition, scAssay)
  cat('checkpoint6', '\n')
  contrast <- colnames(zlmCond@coefD)[2]
  summaryLrt <- summary(zlmCond, doLRT=contrast)
  summaryDt <- summaryLrt
  str(summaryDt)
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
    require(EBSeq)
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
  require(edgeR)
  require(ROTS)
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
    require(statmod)
    require(igraph)
    require(NetSAM)
    require(LPCM)
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

getData1 <- function(ga, ids, start.date = format(Sys.Date() - 8, "%Y-%m-%d"),
                   end.date = format(Sys.Date() - 1, "%Y-%m-%d"), date.format = "%Y-%m-%d",
                   metrics = "ga:users,ga:sessions,ga:pageviews", dimensions = "ga:date",
                   sort = "", filters = "", segment = "", fields = "",
                   start = 1, max, messages = TRUE,  batch, walk = FALSE,
                   output.raw, output.formats, return.url = FALSE, rbr = FALSE, envir = .GlobalEnv,
                   samplingLevel = "HIGHER_PRECISION") {
  
  if (missing(ids)) {
    stop("please enter a profile id")
  }
  
  if (missing(batch) || batch == FALSE) {
    isBatch <- FALSE
    if (missing(max)) {
      # standard
      max <- 1000
    }
  } else {
    isBatch <- TRUE
    if (!is.numeric(batch)) {
      if (!missing(max) && max < 10000) {
        # no need
        batch <- max
      } else {
        # max batch size
        batch <- 10000
      }
    } else {
      if (batch > 10000) {
        # as per https://developers.google.com/analytics/devguides/reporting/core/v3/reference#maxResults
        stop("batch size can be set to max of 10000")
      }
    }
    
    adjustMax <- TRUE
    # arbitrary target, adjust later
    max <- 10000
  }
  
  # ensure that profile id begings with 'ga:'
  if (!grepl("ga:", ids)) {
    ids <- paste("ga:", ids, sep = "")
  }
  
  # remove whitespaces from metrics and dimensions
  metrics <- gsub("\\s", "", metrics)
  dimensions <- gsub("\\s", "", dimensions)
  
  # build url with variables
  url <- "https://www.googleapis.com/analytics/v3/data/ga"
  query <- paste(paste("access_token", ga$getToken()$access_token, sep = "="),
                 paste("ids", ids, sep = "="),
                 paste("start-date", start.date, sep = "="),
                 paste("end-date", end.date, sep = "="),
                 paste("metrics", metrics, sep = "="),
                 paste("dimensions", dimensions, sep = "="),
                 paste("start-index", start, sep = "="),
                 paste("max-results", max, sep = "="),
                 paste("samplingLevel", samplingLevel, sep = "="),
                 sep = "&")
  
  if (sort != "") {
    query <- paste(query, paste("sort", sort, sep = "="), sep = "&")
  }
  if (segment != "") {
    query <- paste(query, paste("segment", segment, sep = "="), sep = "&")
  }
  if (fields != "") {
    query <- paste(query, paste("fields", fields, sep = "="), sep = "&")
  }
  if (filters != "") {
    # available operators
    ops <- c("==", "!=", ">", "<", ">=", "<=", "=@", "!@", "=-", "!-", "\\|\\|", "&&", "OR", "AND")
    # make pattern for gsub
    opsw <- paste("(\\ )+(", paste(ops, collapse = "|"), ")(\\ )+", sep = "")
    # remove whitespaces around operators
    filters <- gsub(opsw, "\\2", filters)
    # replace logical operators
    filters <- gsub("OR|\\|\\|", ",", filters)
    filters <- gsub("AND|&&", ";", filters)
    query <- paste(query, paste("filters", curlEscape(filters), sep = "="), sep = "&", collapse = "")
  }
  
  url <- paste(url, query = query, sep = "?")
  
  if (return.url) {
    return(url)
  }
  
  # thanks to Schaun Wheeler this will not provoke the weird SSL-bug
  if (.Platform$OS.type == "windows") {
    options(RCurlOptions = list(
      verbose = FALSE,
      capath = system.file("CurlSSL", "cacert.pem",
                           package = "RCurl"), ssl.verifypeer = FALSE))
  }
  
  # get data and convert from json to list-format
  # switched to use httr and jsonlite
  request <- GET(url)
  ga.data <- jsonlite::fromJSON(httr::content(request, "text"))
  
  # possibility to extract the raw data
  if (!missing(output.raw)) {
    assign(output.raw, ga.data, envir = envir)
  }
  
  # output error and stop
  if (!is.null(ga.data$error)) {
    stop(paste("error in fetching data: ", ga.data$error$message, sep = ""))
  }
  
  if (ga.data$containsSampledData == "TRUE") {
    isSampled <- TRUE
    if (!walk) {
      message(sprintf("Notice: Data set sampled from %s sessions (%d%% of all sessions)",
                      format(as.numeric(ga.data$sampleSize), big.mark=",", scientific=FALSE),
                      round((as.numeric(ga.data$sampleSize) / as.numeric(ga.data$sampleSpace) * 100))))
    }
  } else {
    isSampled <- FALSE
  }
  
  if (isSampled && walk) {
    return(ga$getDataInWalks(total = ga.data$totalResults, max = max, batch = batch,
                                ids = ids, start.date = start.date, end.date = end.date, date.format = date.format,
                                metrics = metrics, dimensions = dimensions, sort = sort, filters = filters,
                                segment = segment, fields = fields, envir = envir, samplingLevel = samplingLevel))
  }
  
  # check if all data is being extracted
  if (NROW(ga.data$rows) < ga.data$totalResults && (messages || isBatch)) {
    if (!isBatch) {
      message(paste("Only pulling", NROW(ga.data$rows), "observations of", ga.data$totalResults, "total (set batch = TRUE to get all observations)"))
    } else {
      if (adjustMax) {
        max <- ga.data$totalResults
      }
      message(paste("Batch: pulling", max, "observations in batches of", batch))
      # pass variables to batch-function
      return(ga$getDataInBatches(total = ga.data$totalResults, max = max, batchSize = batch,
                                    ids = ids, start.date = start.date, end.date = end.date, date.format = date.format,
                                    metrics = metrics, dimensions = dimensions, sort = sort, filters = filters,
                                    segment = segment, fields = fields, envir = envir, samplingLevel = samplingLevel))
    }
  }
  
  # get column names
  ga.headers <- ga.data$columnHeaders
  # remove ga: from column headers
  ga.headers$name <- sub("ga:", "", ga.headers$name)
  
  # did not return any results
  if (!inherits(ga.data$rows, "matrix") && !rbr) {
    stop(paste("no results:", ga.data$totalResults))
  } else if (!inherits(ga.data$rows, "matrix") && rbr) {
    # If row-by-row is true, return NULL
    return(NULL)
  }
  
  # convert to data.frame
  ga.data.df <- as.data.frame(ga.data$rows, stringsAsFactors = FALSE)
  # insert column names
  names(ga.data.df) <- ga.headers$name
  
  # check if sampled; add attributes if so
  if (isSampled) {
    attr(ga.data.df, "containsSampledData") <- TRUE
    attr(ga.data.df, "sampleSize") <- as.numeric(ga.data$sampleSize)
    attr(ga.data.df, "sampleSpace") <- as.numeric(ga.data$sampleSpace)
  } else {
    attr(ga.data.df, "containsSampledData") <- FALSE
  }
  
  # find formats
  formats <- ga.headers
  
  # convert to r friendly
  formats$dataType[formats$dataType %in% c("INTEGER", "PERCENT", "TIME", "CURRENCY", "FLOAT")] <- "numeric"
  formats$dataType[formats$dataType == "STRING"] <- "character"
  # addition rules
  formats$dataType[formats$name %in% c("latitude", "longitude")] <- "numeric"
  formats$dataType[formats$name %in% c("year", "month", "week", "day", "hour", "minute", "nthMonth", "nthWeek", "nthDay", "nthHour", "nthMinute", "dayOfWeek", "sessionDurationBucket", "visitLength", "daysSinceLastVisit", "daysSinceLastSession", "visitCount", "sessionCount", "sessionsToTransaction", "daysToTransaction")] <- "ordered"
  formats$dataType[formats$name == "date"] <- "Date"
  
  if ("date" %in% ga.headers$name) {
    ga.data.df$date <- format(as.Date(ga.data.df$date, "%Y%m%d"), date.format)
  }
  
  # looping through columns and setting classes
  for (i in 1:nrow(formats)) {
    column <- formats$name[i]
    class <- formats$dataType[[i]]
    if (!exists(paste("as.", class, sep = ""), mode = "function")) {
      stop(paste("can't find function for class", class))
    } else {
      as.fun <- match.fun(paste("as.", class, sep = ""))
    }
    if (class == "ordered") {
      ga.data.df[[column]] <- as.numeric(ga.data.df[[column]])
    }
    ga.data.df[[column]] <- as.fun(ga.data.df[[column]])
  }
  
  # mos-def optimize
  if (!missing(output.formats)) {
    assign(output.formats, formats, envir = envir)
  }
  
  # and we're done
  return(ga.data.df)
}





