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
factorScores <- read.csv('/home/adrose/Brain_Factor_Scores_Volume_GMD_Cortical-Thickness.csv')
fac.scores <- merge(factorScores, vol.data)
factorScoresPerf <- read.csv('/home/adrose/Brain_Factor_Scores_CBF.csv')
fac.scores.perf <- merge(factorScoresPerf, vol.data)
factorScoresFunc <- read.csv('/home/adrose/brain_factor_scores_reho_alff.csv')
fac.scores.func <- merge(factorScoresFunc, vol.data)

all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))
all.fact <- merge(fac.scores, fac.scores.func)
all.fact <- merge(all.fact, fac.scores.perf)

runTpotOnAll <- function(x, y, nFold=10){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  trainSeq <- seq(1, length(y))
  # Now we need to loop thorugh each fold and get our output fit stats
  #cl <- makeCluster(20)
  #registerDoParallel(cl)
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- as.matrix(x)[index,]
    trainY <- as.vector(y)[index]
    testX <- as.matrix(x)[-index,]

    # Run variable selection
    varSelectDF <- as.data.frame(cbind(trainY, trainX))
    #varSelectDF$sex <- 1
    colnames(varSelectDF)[1] <- 'F1_Exec_Comp_Cog_Accuracy'
    #selectSums <- returnSelectionN(dataFrame=varSelectDF, grepID='_jlf_', genderID=1, iterationCount=100, nCor=31)
    #valsToUse <- which(as.numeric(returnSelectionCol(selectSums)[,2]) > 24)
    #nameVals <- rownames(selectSums)[valsToUse]
    #colVals <- which(colnames(trainX) %in% nameVals)
    
    # Now apply the selection
    #trainX <- trainX[,colVals]
    #testX <- testX[,colVals]  
    #pVal <- dim(trainX)[2]
    #varSelectDF <- varSelectDF[,c(1, which(names(varSelectDF) %in% colnames(trainX)))]

    # Build a regression tree
    #tc <- trainControl("cv", number=4)
    #Grid <- expand.grid(mtry = seq(4,16,4))
    colNamesFreeze <- colnames(varSelectDF)[-1]
    tree1 <- rpart(F1_Exec_Comp_Cog_Accuracy~.,data=varSelectDF, method='anova',control=rpart.control(minsplit=10,maxdepth=1))
    trainX <- cbind(trainX, predict(tree1))
    newPredVals <- as.data.frame(testX)
    colnames(newPredVals) <- colNamesFreeze
    testX <- cbind(testX, predict(tree1, newdata=newPredVals))

    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0,nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, testX)
  }
  #stopCluster(cl)
  # Now return the output CvVals
  output <- outputCvValsR
  return(output)
}

dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','fac.scores', 'fac.scores', 'fac.scores.func', 'fac.scores.func','all.data', 'all.fact', 'reho.data', 'alff.data', 'ct.data', 'cc.data', 'fac.scores.perf', 'fac.scores')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'facvol', 'facct', 'facreho', 'facalff', 'all.dat', 'all.fac', 'reho', 'alff', 'ct', 'cc', 'faccbf', 'facgmd')
grepValue <- c(rep('_jlf_', 4), 'Volume_', 'Cortical_Thickness', 'ReHo_', 'ALFF_', '_jlf_', '_F', '_jlf_', '_jlf_', '_jlf_', '_jlf_', 'CBF_', 'GMD_')
allR <- NULL
for(q in seq(1,100)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 5)
    corVal <- cor(predVals, tmpDatY)
    outRow <- c(outName[i], q, corVal)
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}
rownames(allR) <- NULL
allRVals <- as.data.frame(allR)
allRVals$V3 <- as.numeric(as.character(allRVals$V3))
foo <- summarySE(allRVals, measurevar='V3', groupvars='V1')
foo <- foo[order(foo$V3, decreasing=T),]
allRVals$V1 <- factor(allRVals$V1, levels=foo$V1)
outPlot <- ggplot(allRVals, aes(x=V1, y=as.numeric(as.character(V3))^2)) + 
  geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + 
  labs(title='CV Prediction by modality', y='CV R-squared', x='Modality')
