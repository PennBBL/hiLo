# AFGR June 2017

# This script will be used to produce intermodal heat maps for the hi lo project

#Load library(s)
source("/home/adrose/hiLo/scripts/06_IntermodalCor/functions/functions.R")
install_load('ggplot2','scales', 'corrplot', 'psych', 'reshape2')

## Load data here
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

# Now merge all of the data
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)), all=T)
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)), all=T)
all.data <- merge(all.data, ct.data, by=intersect(names(all.data), names(ct.data)), all=T)
all.data <- merge(all.data, reho.data, by=intersect(names(all.data), names(reho.data)), all=T)
all.data <- merge(all.data, alff.data, by=intersect(names(all.data), names(alff.data)), all=T)
all.data <- merge(all.data, ad.data, by=intersect(names(all.data), names(ad.data)), all=T)
all.data <- merge(all.data, fa.data, by=intersect(names(all.data), names(fa.data)), all=T)
all.data <- merge(all.data, rd.data, by=intersect(names(all.data), names(rd.data)), all=T)
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)), all=T)


# Now create a function which will produce the heat maps and do everything ever
createHeatMap <- function(grepPattern1, grepPattern2){
  # First merge the two dataFrames
  matVals1 <- all.data[,grep(grepPattern1, names(all.data))]
  matVals2 <- all.data[,grep(grepPattern2, names(all.data))]

  # Now reorder the data 
  matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
  matVals2 <- matVals2[,order(outputLobeRow(matVals2))]

  # Now nonsense lobes
  matVals1 <- matVals1[,-which(outputLobeRow(matVals1)>=7)]
  matVals2 <- matVals2[,-which(outputLobeRow(matVals2)>=7)]

  # Now rm outliers
  #matVals1 <- data.matrix(as.data.frame(apply(matVals1, 2, function(x) rmOutliers(x, 3))))
  #matVals2 <- data.matrix(as.data.frame(apply(matVals2, 2, function(x) rmOutliers(x, 3))))

  # Now find the intersection of names and ensure that we only have regions that both DF have
  colNamesMV1 <- gsub(x=colnames(matVals1), pattern=grepPattern1, replacement='')
  colNamesMV2 <- gsub(x=colnames(matVals2), pattern=grepPattern2, replacement='')
  intersectVals <- intersect(colNamesMV1, colNamesMV2)
  lengthValue <- length(intersectVals)
  if(lengthValue!=dim(matVals1)[2]){
    matVals1 <- matVals1[,colNamesMV1 %in% intersectVals]
  }  
  if(lengthValue!=dim(matVals2)[2]){
    matsVals2 <- matVals2[,colNamesMV2 %in% intersectVals]
  }

  # Now plot our heat map!
  corMatrix <- cor(matVals1, matVals2, use='complete')
  maxVal <- max(corMatrix)
  minVal <- min(corMatrix)
  corMatrix <- melt(corMatrix)
  levels(corMatrix$Var1) <- colnames(matVals1)
  levels(corMatrix$Var2) <- colnames(matVals2)
  output <- qplot(x=Var1, y=Var2, data=corMatrix, fill=value, geom="tile") + 
    theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    scale_fill_gradient2(low="blue", high="red", limits=c(minVal, maxVal)) + 
    coord_equal()
  return(output)  
}

# Now produce all of the heat maps
grepPattern1 <- c('mprage_jlf_vol', 'pcasl_jlf_cbf', 'mprage_jlf_ct', 'mprage_jlf_gmd', 'rest_jlf_reho', 'rest_jlf_alff', 'dti_jlf_ad', 'dti_jlf_rd', 'dti_jlf_tr')
for(p in 1:length(grepPattern1)){
  modalName <- rev(strSplitMatrixReturn(grepPattern1[p], '_'))[1]
  pdf(paste(modalName, '-heatMaps.pdf', sep=''), width=20, height=20)
  for(n in 1:length(grepPattern1)){
    tmp <- createHeatMap(grepPattern1[p], grepPattern1[n]) 
    print(tmp) 
  }
  dev.off()
}
