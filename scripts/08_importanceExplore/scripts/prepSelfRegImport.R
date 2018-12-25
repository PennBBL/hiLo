## Load packages
source('~/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart', 'stir')

## Load the data
vol.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv')
cbf.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/cbfData.csv')
gmd.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/gmdData.csv')
tr.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfTRData.csv')
alff.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/alffData.csv')
reho.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/rehoData.csv')
fa.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jhuFATracts.csv')
colnames(fa.data) <- gsub(x=colnames(fa.data), pattern='jhutract', replacement='jlf')
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
output.values <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- as.matrix(scale(tmpDat[,grep(grepValue[i], names(tmpDat))]))
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  ## Now train a ridge reg model
  modelFit <- lm(tmpDatY~tmpDatX)
  coef.vals <- coef(modelFit)[-1]
  to.store <-  cbind(rownames(cbind(coef.vals, rep(outName[i], length(coef.vals)))), coef.vals, rep(outName[i], length(coef.vals)))
  output.values <- rbind(output.values, to.store)
}
## Now write the output
rownames(output.values) <- NULL
output.values <- cbind(output.values, rep("LMCoef", dim(output.values)[1]))
write.csv(output.values, "selfRegImpMale.csv", quote=F, row.names=F)

## Now do females
output.values <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- as.matrix(scale(tmpDat[,grep(grepValue[i], names(tmpDat))]))
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  ## Now train a ridge reg model
  modelFit <- lm(tmpDatY~tmpDatX)
  coef.vals <- coef(modelFit)[-1]
  to.store <-  cbind(rownames(cbind(coef.vals, rep(outName[i], length(coef.vals)))), coef.vals, rep(outName[i], length(coef.vals)))
  output.values <- rbind(output.values, to.store)
}
## Now write the output
rownames(output.values) <- NULL
output.values <- cbind(output.values, rep("LMCoef", dim(output.values)[1]))
write.csv(output.values, "selfRegImpFemale.csv", quote=F, row.names=F)
