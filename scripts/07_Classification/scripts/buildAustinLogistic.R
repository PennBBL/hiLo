source('/home/adrose/hiLo/scripts/07_Classification/functions/functions.R')
install_load('psych','foreach', 'doParallel', 'caret', 'glmnet', 'pROC')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2')


# Load data
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
tr.data$dti_jlf_tr_MeanTR <- apply(tr.data[,grep('dti_jlf_tr_', names(tr.data))], 1, mean)

all.data <- merge(vol.data, cbf.data)
all.data <- merge(all.data, gmd.data)
all.data <- merge(all.data, tr.data)

# Now build all of this logic into a loop
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'all.data')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_', '_jlf_')
allAUCVals <- NULL
modelVals <- NULL
cvAUCVals <- NULL
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.33,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 1
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 2
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 3
  return(data)
}

tmpDF <- get(dataNames[1])
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
allOut <- outCol

for(z in 2:5){
  tmpDF <- get(dataNames[z])
  tmpDF1 <- tmpDF[which(tmpDF$sex==1),]
  tmpDF2 <- tmpDF[which(tmpDF$sex==2),]
  tmpDF1 <- returnPerfBin(tmpDF1)
  tmpDF2 <- returnPerfBin(tmpDF2)
  outCol1 <- tmpDF1[,c('bblid','scanid','perfBin')]
  outCol2 <- tmpDF2[,c('bblid','scanid','perfBin')]
  outCol <- rbind(outCol1, outCol2)
  colnames(outCol)[3] <- paste('perfCol', z, sep='')
  allOut <- merge(allOut, outCol, all=T)
}
write.csv(allOut, '~/perfBinsForRUben.csv', quote=F, row.names=F)

for(g in 1:2){
  for(z in 1:4){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmpDF <- returnPerfBin(tmpDF)
    tmpDFIndex <- createFolds(y=tmpDF$perfBin, k=5, list=T, returnTrain=T)
    tmpDFTest <- tmpDF[-tmpDFIndex[[1]],]
    tmpDF <- tmpDF[tmpDFIndex[[1]],]
    save(tmpDFIndex, file=paste(g,z,'TrainIndex.RData', sep=''))
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$perfBin)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    selectN <- returnSelectionN(dataFrame=tmpDF, grepID='_jlf_', genderID=g, iterationCount=100, nCor=25)
    modelOut <- buildAustinModel(selectN, tmp.values, tmp.out, breakValue=1.1, nIters=dim(selectN)[1])    
    print(plot(modelOut[[2]], main=dataNames[z], ylab='CV AUC'))
    allAUCVals <- rbind(allAUCVals, modelOut[[2]][which(modelOut[[2]]==max(modelOut[[2]]))])
    modelVals <- rbind(modelVals, modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])   
    tmpModel <- glm(as.formula(paste('perfBin ~', modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])), data=tmpDF)
    trainX <- as.matrix(tmpModel$model[,2:dim(tmpModel$model)[2]])
    trainY <- tmpModel$model[,1]
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=10^seq(3, -2, by = -.1), nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)
    newRoc <- roc(tmpDFTest$perfBin ~ as.numeric(predict(modelFit, as.matrix(tmpDFTest[,colnames(tmpDFTest) %in% colnames(trainX)]), type='response')))
    cvAUCVals <- rbind(cvAUCVals, cbind(g, z, auc(newRoc)))
  }
}
write.csv(cvAUCVals, 'aucVals.csv', quote=F, row.names=F)


# Now group hi vs lo
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.34,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 1
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 7
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 0
  data <- data[-which(data$perfBin==7),]
  return(data)
}
for(g in 1:2){
  for(z in 1:4){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmpDF <- returnPerfBin(tmpDF)
    tmpDFIndex <- createFolds(y=tmpDF$perfBin, k=5, list=T, returnTrain=T)
    tmpDFTest <- tmpDF[-tmpDFIndex[[1]],]
    tmpDF <- tmpDF[tmpDFIndex[[1]],]
    save(tmpDFIndex, file=paste(g,z,'TrainIndex.RData', sep=''))
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$perfBin)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    selectN <- returnSelectionN(dataFrame=tmpDF, grepID='_jlf_', genderID=g, iterationCount=100, nCor=25)
    modelOut <- buildAustinModel(selectN, tmp.values, tmp.out, breakValue=1.1, nIters=dim(selectN)[1])    
    print(plot(modelOut[[2]], main=dataNames[z], ylab='CV AUC'))
    allAUCVals <- rbind(allAUCVals, modelOut[[2]][which(modelOut[[2]]==max(modelOut[[2]]))])
    modelVals <- rbind(modelVals, modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])   
    tmpModel <- glm(as.formula(paste('perfBin ~', modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])), data=tmpDF)
    trainX <- as.matrix(tmpModel$model[,2:dim(tmpModel$model)[2]])
    trainY <- tmpModel$model[,1]
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=10^seq(3, -2, by = -.1), nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)
    newRoc <- roc(tmpDFTest$perfBin ~ as.numeric(predict(modelFit, as.matrix(tmpDFTest[,colnames(tmpDFTest) %in% colnames(trainX)]), type='response')))
    cvAUCVals <- rbind(cvAUCVals, cbind(g, z, auc(newRoc)))
  }
}
