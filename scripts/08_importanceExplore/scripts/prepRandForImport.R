## Load library(s)
source('~/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart', 'randomForest')

## Now load the data 
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

## Register a parallel backend
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 16
set.seed(seed)
dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','fa.data','all.data', 'reho.data', 'alff.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'fa', 'all.data', 'reho', 'alff')
grepValue <- c(rep('_jlf_', 7), '_jlf_')
to.write <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- scale(tmpDat[,grep(grepValue[i], names(tmpDat))])
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  trainData <- data.frame(tmpDatY,tmpDatX)
  mtry <- sqrt(ncol(tmpDatX))
  tunegrid <- expand.grid(.mtry=mtry)
  rf_default <- train(tmpDatY~., data=trainData, method="rf", tuneGrid=tunegrid, trControl=control)
  tmpMod <- randomForest(x=tmpDatX, y=tmpDatY, mtry=rf_default$bestTune$mtry, importance=T)
  out.vals <- cbind(rownames(tmpMod$importance), scale(tmpMod$importance[,1]),rep(outName[i], length(tmpMod$importance[,1])))
  to.write <- rbind(to.write, out.vals)
  print(paste("Done with", outName[i]))
}
# Now clean and write the output file
rownames(to.write) <- NULL
to.write <- cbind(to.write, rep("RandomForest", dim(to.write)[1]))
write.csv(to.write, "randForImpMale.csv", quote=F, row.names=F)
## Now do females
to.write <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==2),]
  tmpDatX <- scale(tmpDat[,grep(grepValue[i], names(tmpDat))])
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  trainData <- data.frame(tmpDatY,tmpDatX)
  mtry <- sqrt(ncol(tmpDatX))
  tunegrid <- expand.grid(.mtry=mtry)
  rf_default <- train(tmpDatY~., data=trainData, method="rf", tuneGrid=tunegrid, trControl=control)
  tmpMod <- randomForest(x=tmpDatX, y=tmpDatY, mtry=rf_default$bestTune$mtry, importance=T)
  out.vals <- cbind(rownames(tmpMod$importance), tmpMod$importance[,1],rep(outName[i], length(tmpMod$importance[,1])))
  to.write <- rbind(to.write, out.vals)
  print(paste("Done with", outName[i]))
}
# Now clean and write the output file
rownames(to.write) <- NULL
to.write <- cbind(to.write, rep("RandomForest", dim(to.write)[1]))
write.csv(to.write, "randForImpFemale.csv", quote=F, row.names=F)
