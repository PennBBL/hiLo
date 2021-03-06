## This script runs on Adon's MBP!
## STIR requires R version > 3.3, Galton currently runs R 3.3


## Load packages
source('~/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart', 'stir')

## Load the data
vol.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
alff.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
reho.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
fa.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabel.csv')
colnames(fa.data) <- gsub(x=colnames(fa.data), pattern='jhulabel', replacement='jlf')
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))
all.data <- merge(all.data, fa.data)
all.data <- merge(all.data, alff.data)
all.data <- merge(all.data, reho.data)

## Now lets grab our reliefF importance metrics for each modality
dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','fa.data','all.data', 'reho.data', 'alff.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'fa', 'all.data', 'reho', 'alff')
grepValue <- c(rep('_jlf_', 7), '_jlf_')
RF.method = "multisurf"
metric <- "euclidean"
output.values <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- scale(tmpDat[,grep(grepValue[i], names(tmpDat))])
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  ## Now run relief F on these guys
  tmpDatY2<- tmpDatY
  tmpDatY2[tmpDatY2>0.2397024] <- 1
  tmpDatY2[tmpDatY2<=0.2397024] <- 0
  neighbor.idx.observed <- find.neighbors(tmpDatX, tmpDatY2, k = 0, method = RF.method)
  results.list <- stir(tmpDatX, neighbor.idx.observed, k = k, metric = metric, method = RF.method)
  t_sorted_multisurf <- results.list$STIR_T$t.stat
  t_sorted_multisurf <- cbind(rownames(results.list$STIR_T), t_sorted_multisurf, rep(outName[i], dim(tmpDatX)[2]))
  output.values <- rbind(output.values, t_sorted_multisurf)
}
## Now write the output
rownames(output.values) <- NULL
output.values <- cbind(output.values, rep("ReliefF", dim(output.values)[1]))
write.csv(output.values, "reliefFImpMale.csv", quote=F, row.names=F)

## Now do it for females
output.values <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==2),]
  tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  ## Now run relief F on these guys
  tmpDatY2<- tmpDatY
  tmpDatY2[tmpDatY2>0.2397024] <- 1
  tmpDatY2[tmpDatY2<=0.2397024] <- 0
  neighbor.idx.observed <- find.neighbors(tmpDatX, tmpDatY2, k = 0, method = RF.method)
  results.list <- stir(tmpDatX, neighbor.idx.observed, k = k, metric = metric, method = RF.method)
  t_sorted_multisurf <- results.list$STIR_T$t.stat
  t_sorted_multisurf <- cbind(rownames(results.list$STIR_T), t_sorted_multisurf, rep(outName[i], dim(tmpDatX)[2]))
  output.values <- rbind(output.values, t_sorted_multisurf)
}
## Now write the output
rownames(output.values) <- NULL
output.values <- cbind(output.values, rep("ReliefF", dim(output.values)[1]))
write.csv(output.values, "reliefFImpFemale.csv", quote=F, row.names=F)
