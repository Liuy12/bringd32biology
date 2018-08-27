cranpacks <- c('devtools','shiny', 'shinydashboard', 'magrittr', 'shinythemes', 'colorspace', 'doParallel', 'ggplot2', 'scatterplot3d', 'GGally', 'data.table', 'dplyr', 'threejs', 'd3heatmap', 'metricsgraphics', 'networkD3', 'DT', 'statmod', 'igraph', 'LPCM')
biocpacks <- c('RUVSeq', 'DESeq', 'DESeq2', 'XBSeq', 'edgeR', 'limma', 'scde', 'monocle', 'EBSeq', 'NetSAM', 'MAST', 'ROTS')

library(BiocInstaller)
for(i in cranpacks){
  if(! i %in% installed.packages()[,1])
    install.packages(i)
}
for(i in biocpacks){
  if(! i %in% installed.packages()[,1])
    biocLite(i)
}

if(!"slidify" %in% installed.packages()[,1])
  devtools::install_github("slidify", "ramnathv")
if(!'slidifyLibraries' %in% installed.packages()[,1])
  devtools::install_github("slidifyLibraries", "ramnathv")
if(!'rCharts' %in% installed.packages()[,1])
  devtools::install_github("rCharts", "ramnathv")
if(!'BPSC' %in% installed.packages()[,1])
  devtools::install_github("BPSC","nghiavtr")
if(!'rga' %in% installed.packages()[,1])
  devtools::install_github("rga", "skardhamar")
