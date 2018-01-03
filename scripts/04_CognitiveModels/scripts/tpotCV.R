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
fa.data.wm <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFATractsData.csv')
tr.data$dti_jlf_tr_MeanTR <- apply(tr.data[,grep('dti_jlf_tr_', names(tr.data))], 1, mean)

# Create a reho volume weighted variable
rehoVals <- reho.data[,grep('_jlf_', names(reho.data))]
volVals <- merge(vol.data, reho.data)
volVals <- volVals[,grep('mprage_jlf_vol_', names(volVals))] 
tmpNamesReho <- gsub(names(rehoVals), pattern='rest_jlf_reho_', replacement= '')
tmpNamesVol <- gsub(names(volVals), pattern='mprage_jlf_vol_', replacement= '')
volVals <- volVals[,tmpNamesVol %in% tmpNamesReho]
rehoOutVals <- NULL
for(q in 1:905){
  weightedVal <- weighted.mean(rehoVals[q,], volVals[q,])
  rehoOutVals <- append(rehoOutVals, weightedVal)
}
alffVals <- alff.data[,grep('_jlf_', names(alff.data))]
alffOutVals <- NULL
for(q in 1:905){
  weightedVal <- weighted.mean(alffVals[q,], volVals[q,])
  alffOutVals <- append(alffOutVals, weightedVal)
}
allOut <- cbind(reho.data[,c(1, 21)], rehoOutVals, alffOutVals)

all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))

runTpotOnAll <- function(x, y, nFold=10, grepID){
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

    # Now protext agains our factor analysis results
    if(identical(sumValIndex, integer(0))){
      varSelectDF <- as.data.frame(cbind(trainY, trainX))
    } else{
      varSelectDF <- as.data.frame(cbind(trainY, trainX[,-sumValIndex]))
    }

    # Run variable selection
    varSelectDF$sex <- 1
    colnames(varSelectDF)[1] <- 'F1_Exec_Comp_Cog_Accuracy'
    #selectSums <- returnSelectionN(dataFrame=varSelectDF, grepID=grepID, genderID=1, iterationCount=100, nCor=31)
    
    # Now explore different cut off values
    #valsToUse <- which(as.numeric(returnSelectionCol(selectSums)[,2]) > 24)
    #nameVals <- rownames(selectSums)[valsToUse]
    #colVals <- which(colnames(trainX) %in% nameVals)
    #colVals <- c(colVals, sumValIndex)
    
    # Now apply the selection
    #trainX <- trainX[,colVals]
    #testX <- testX[,colVals]  
    #pVal <- dim(trainX)[2]
    #varSelectDF <- varSelectDF[,c(1, which(names(varSelectDF) %in% colnames(trainX)))]

    # Build a regression tree
    #tree1 <- rpart(F1_Exec_Comp_Cog_Accuracy~.,data=trainX, method='anova',control=rpart.control(minsplit=10,maxdepth=1))
    #trainX <- cbind(trainX, predict(tree1))
    #testX <- cbind(testX, predict(tree1, newdata=as.data.frame(testX)))

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

dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','all.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'all.dat')
grepValue <- c(rep('_jlf_', 4), '_jlf_')
allR <- NULL
for(q in seq(1,100)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    corVal <- cor(predVals[[1]], tmpDatY)^2
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, cvICC, cvRMSE)
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}

write.csv(allR, 'tmpAllRValsMale.csv', quote=F, row.names=F)

# Now do this for females
allR <- NULL
for(q in seq(1,100)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==2),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    corVal <- cor(predVals[[1]], tmpDatY)^2
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, cvICC, cvRMSE)
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}


write.csv(allR, 'tmpAllRValsFemale.csv', quote=F, row.names=F)
