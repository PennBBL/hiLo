source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')

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

all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))

runTpotOnAll <- function(x, y, nFold=10){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  trainSeq <- seq(1, length(y))
  modelOut <- matrix(0, dim(x)[2], nFold)
  rownames(modelOut) <- colnames(x)
  # Now we need to loop thorugh each fold and get our output fit stats
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- as.matrix(x)[index,]
    trainY <- as.vector(y)[index]
    testX <- as.matrix(x)[-index,]

    # Now get the summary metrics
    sumValIndex <- grep('ICV', colnames(trainX))
    sumValIndex <- append(sumValIndex, grep('Mean', colnames(testX)))

    # Run variable selection
    varSelectDF <- as.data.frame(cbind(trainY, trainX[,-sumValIndex]))
    varSelectDF$sex <- 1
    colnames(varSelectDF)[1] <- 'F1_Exec_Comp_Cog_Accuracy'
    selectSums <- returnSelectionN(dataFrame=varSelectDF, grepID='_jlf_', genderID=1, iterationCount=100, nCor=31)
    valsToUse <- which(as.numeric(returnSelectionCol(selectSums)[,2]) > 24)
    nameVals <- rownames(selectSums)[valsToUse]
    colVals <- which(colnames(trainX) %in% nameVals)
    colVals <- c(colVals, sumValIndex)
    
    # Now apply the selection
    trainX <- trainX[,colVals]
    testX <- testX[,colVals]  
    pVal <- dim(trainX)[2]
    varSelectDF <- varSelectDF[,c(1, which(names(varSelectDF) %in% colnames(trainX)))]

    # Build a regression tree
    tree1 <- rpart(F1_Exec_Comp_Cog_Accuracy~.,data=varSelectDF, method='anova',control=rpart.control(minsplit=10,maxdepth=1))
    trainX <- cbind(trainX, predict(tree1))
    testX <- cbind(testX, predict(tree1, newdata=as.data.frame(testX)))

    # Now scale the data
    trainX <- as.matrix(scale(trainX))
    testX <- as.matrix(scale(testX))

    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0,nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)
    modelOut[rownames(modelOut) %in% rownames(coef(modelFit)),i] <- 1

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, testX)
  }

  # Now return the output CvVals
  output <- list()
  output[[1]] <- outputCvValsR
  output[[2]] <- modelOut
  return(output)
}

allR <- NULL
for(z in seq(1,10)){
for(i in c('vol.data','cbf.data','gmd.data','tr.data','all.data')){
  tmpDat <- get(i)
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- tmpDat[,grep('_jlf_', names(tmpDat))]
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10)
  corVal <- cor(predVals[[1]], tmpDatY)
  outRow <- c(i, z, corVal)
  allR <- rbind(allR, outRow)
  write.csv(allR, 'tmpAllRValsFMale.csv', quote=F, row.names=F)
  # Now write a csv for selected Values
  write.csv(predVals[[2]], paste(z, i, 'SelectVals.csv', sep=''), quote=F)
}
}

write.csv(allR, 'tmpAllRValsMale.csv', quote=F, row.names=F)

# Now do this for females
allR <- NULL
for(z in seq(1,10)){
for(i in c('vol.data','cbf.data','gmd.data','tr.data','all.data')){
  tmpDat <- get(i)
  tmpDat <- tmpDat[which(tmpDat$sex==2),]
  tmpDatX <- tmpDat[,grep('_jlf_', names(tmpDat))]
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10)
  corVal <- cor(predVals[[1]], tmpDatY)
  outRow <- c(i, z, corVal)
  allR <- rbind(allR, outRow)
  write.csv(allR, 'tmpAllRValsFFemale.csv', quote=F, row.names=F)
  # Now write a csv for selected Values
  write.csv(predVals[[2]], paste(z, i, 'SelectVals.csv', sep=''), quote=F)
}
}

write.csv(allR, 'tmpAllRValsMale.csv', quote=F, row.names=F)
