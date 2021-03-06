source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')

vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/ccData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfTRData.csv')
fa.data.wm <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jhuFALabel.csv')
#fa.data.wm <- fa.data.wm[-863,]
fa.data.wm$dti_dtitk_jhutract_fa_MeanFA <- apply(fa.data.wm[,grep('dti_dtitk_jhutract_fa_', names(fa.data.wm))], 1, mean)
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
  # FInally scale our data
  inputX <- as.matrix(scale(x))
  inputY <- as.matrix(scale(y))

  # Now we need to loop thorugh each fold and get our output fit stats
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- inputX[index,]
    trainY <- inputY[index]
    testX <- as.matrix(inputX)[-index,]

    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0,nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)
    modelOut[rownames(modelOut) %in% rownames(coef(modelFit)),i] <- 1

    # Now get our prediction values in the test values
    outputCvValsR[-index] <- predict(modelFit, data.matrix(testX))
  }
  # Now return the output CvVals
  output <- list()
  output[[1]] <- outputCvValsR
  output[[2]] <- modelOut
  return(output)
}

factor.names <- names(vol.data)[4:16]
dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','all.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'all.dat')
grepValue <- c(rep('_jlf_', 4), '_jlf_')
allR <- NULL
for(f in factor.names){
for(q in seq(1,10)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- as.vector(tmpDat[,f])
    predVals <- runTpotOnAll(tmpDatX, tmpDatY,10, grepValue[i])
    corVal <- cor(predVals[[1]], tmpDatY)^2
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(f, i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}
}
write.csv(allR, "AllEverythingForEever.csv", quote=F, row.names=F)
orig <- allR
allR <- as.data.frame(allR)
allR$V4 <- as.numeric(as.character(allR$V4))
allR <- dcast(V3 ~ V2 + V1, data=allR, value.var='V4')
outMean <- round(apply(allR, 2, mean), digits=2)
outMedian <- round(apply(allR, 2, median), digits=2)
outMin <- round(apply(allR, 2, min), digits=2)
outMax <- round(apply(allR, 2, max), digits=2)
output <- cbind(orig[1:5,4], orig[1:5,5], outMean[2:6], outMedian[2:6], outMin[2:6], outMax[2:6])
write.csv(output, 'tmpAllRValsMale.csv', quote=F, row.names=F)

# Now do this for females
allR <- NULL
for(q in seq(1,10)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==2),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, dim(tmpDatX)[1], grepValue[i])
    corVal <- cor(predVals[[1]], tmpDatY)^2
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}
orig <- allR
allR <- as.data.frame(allR)
allR <- dcast(V2 ~ V1, data=allR, value.var='V3')
outMean <- round(apply(allR, 2, mean), digits=2)
outMedian <- round(apply(allR, 2, median), digits=2)
outMin <- round(apply(allR, 2, min), digits=2)
outMax <- round(apply(allR, 2, max), digits=2)
output <- cbind(orig[1:5,4], orig[1:5,5], outMean[2:6], outMedian[2:6], outMin[2:6], outMax[2:6])
write.csv(output, 'tmpAllRValsFemale.csv', quote=F, row.names=F)
