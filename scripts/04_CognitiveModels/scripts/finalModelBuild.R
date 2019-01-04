source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2')

# Source functions and what not
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.34,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 3
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 1
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 1
  return(data)
}

# Now run variable selection with the modality regressed variables and see if this alters our selection at all
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

# Now prepare the all data values 
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
#all.data <- merge(all.data, cc.data, by=intersect(names(all.data), names(cc.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))

## This script is going to be used to build ridge regression models in a cv'ed manner
## The important aspect is the twice 10 fold cross validation.
## Models will be trained within the training 9 folds in a 10 -fold cv manner, and then validated in the left out
## 1/10th fold - 10 times.

## Now create a loop to go though each image modality, gender, and return the CVR^2
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'all.data')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_', '_jlf_')


# Now try austin on all
# First remove the summary variables
all.data.var.select <- all.data
valsToRM <- grep('_Mean', names(all.data.var.select))
valsToRM <- c(grep('_ICV', names(all.data.var.select)), valsToRM)
all.data.var.select <- all.data.var.select[,-valsToRM]
vol.data.step <- vol.data[,-grep('mprage_jlf_vol_ICV', names(vol.data))]
cbf.data.step <- cbf.data[,-grep('pcasl_jlf_cbf_MeanGM', names(cbf.data))]
gmd.data.step <- gmd.data[,-grep('mprage_jlf_gmd_MeanGMD', names(gmd.data))]
tr.data.step <- tr.data[,-grep('dti_jlf_tr_MeanTR', names(tr.data))]

# Now try the step wise austin heirarchy model building 
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'all.data')
data.step <- c('vol.data.step', 'cbf.data.step', 'gmd.data.step', 'tr.data.step', 'all.data.var.select')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_', '_jlf_')
allRVals <- NULL
modelVals <- NULL
cvRVals <- NULL
pdf('outCVTrends.pdf')
for(g in 1:2){
  for(z in 1:5){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmpDFIndex <- createFolds(y=tmpDF$F1_Exec_Comp_Cog_Accuracy, k=5, list=T, returnTrain=T)
    tmpDFTest <- tmpDF[-tmpDFIndex[[1]],]
    tmpDF <- tmpDF[tmpDFIndex[[1]],]
    save(tmpDFIndex, file=paste(g,z,'TrainIndex.RData', sep=''))
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    # Produce the selection count 
    selectN <- returnSelectionN(dataFrame=get(data.step[z])[tmpDFIndex[[1]],], grepID=dataGrepNames[z], genderID = g, nCor=30, iterationCount=1000)
    write.csv(selectN, paste(g,z,'selectVals.csv', sep=''), quote=F, row.names=T)
    #selectN <- read.csv(paste(g,z,'selectVals.csv', sep=''))
    selectN[,1] <- as.character(selectN[,1])
    rownames(selectN) <- selectN[,1]
    # Now get all of the R^2 values
    modelOut <- buildAustinModel(selectN, predVals=tmp.values, outVals=tmp.out, breakValue=1.1, nIters=dim(selectN)[1], stepSize=1)
    print(plot(modelOut[[2]], main=data.step[z], ylab='CV R-Squared'))
    allRVals <- rbind(allRVals, modelOut[[2]][which(modelOut[[2]]==max(modelOut[[2]]))])
    modelVals <- rbind(modelVals, modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])
    # Now get the CV R-squared value again
    tmpModel <- lm(as.formula(paste('F1_Exec_Comp_Cog_Accuracy ~', modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])), data=tmpDF)
    trainX <- as.matrix(tmpModel$model[,2:dim(tmpModel$model)[2]])
    trainY <- tmpModel$model[,1]
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=10^seq(3, -2, by = -.1), nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)
    newValues <- cor(tmpDFTest$F1_Exec_Comp_Cog_Accuracy, predict(modelFit, as.matrix(tmpDFTest[,colnames(tmpDFTest) %in% colnames(trainX)])))^2
    cvRVals <- rbind(cvRVals, cbind(g, z, newValues))
  }
}
dev.off()
write.csv(allRVals, 'finalFitStats.csv', quote=F)
write.csv(modelVals, 'finalModelVals.csv', quote=F)
write.csv(cvRVals, 'finalCVFitStats.csv', quote=F)

# Now look into classification
# Now try the step wise austin heirarchy model building 
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'all.data')
data.step <- c('vol.data.step', 'cbf.data.step', 'gmd.data.step', 'tr.data.step', 'all.data.var.select')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_', '_jlf_')
allRVals <- NULL
modelVals <- NULL
cvRVals <- NULL
pdf('outCVTrends.pdf')
for(g in 1:2){
  for(z in 1:5){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmpDF <- returnPerfBin(tmpDF)
    tmpDFIndex <- createFolds(y=tmpDF$F1_Exec_Comp_Cog_Accuracy, k=5, list=T, returnTrain=T)
    tmpDFTest <- tmpDF[-tmpDFIndex[[1]],]
    tmpDF <- tmpDF[tmpDFIndex[[1]],]
    save(tmpDFIndex, file=paste(g,z,'TrainIndex.RData', sep=''))
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    # Produce the selection count 
    selectN <- returnSelectionN(dataFrame=get(data.step[z])[tmpDFIndex[[1]],], grepID=dataGrepNames[z], genderID = g, nCor=30, iterationCount=1000)
    write.csv(selectN, paste(g,z,'selectVals.csv', sep=''), quote=F, row.names=T)
    #selectN <- read.csv(paste(g,z,'selectVals.csv', sep=''))
    selectN[,1] <- as.character(selectN[,1])
    rownames(selectN) <- selectN[,1]
    # Now get all of the R^2 values
    modelOut <- buildAustinModel(selectN, predVals=tmp.values, outVals=tmp.out, breakValue=1.1, nIters=dim(selectN)[1], stepSize=1)
    print(plot(modelOut[[2]], main=data.step[z], ylab='CV R-Squared'))
    allRVals <- rbind(allRVals, modelOut[[2]][which(modelOut[[2]]==max(modelOut[[2]]))])
    modelVals <- rbind(modelVals, modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])
    # Now get the CV R-squared value again
    tmpModel <- lm(as.formula(paste('F1_Exec_Comp_Cog_Accuracy ~', modelOut[[3]][which(modelOut[[2]]==max(modelOut[[2]]))])), data=tmpDF)
    trainX <- as.matrix(tmpModel$model[,2:dim(tmpModel$model)[2]])
    trainY <- tmpModel$model[,1]
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=10^seq(3, -2, by = -.1), nfolds=10, family="binomial")
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal, family="binomial")
    newValues <- cor(tmpDFTest$F1_Exec_Comp_Cog_Accuracy, predict(modelFit, as.matrix(tmpDFTest[,colnames(tmpDFTest) %in% colnames(trainX)])))^2
    cvRVals <- rbind(cvRVals, cbind(g, z, newValues))
  }
}
