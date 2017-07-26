# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'e1071')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
vol.data <- vol.data[,-30]
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
fa.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabelsData.csv')
tr.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuTRLabelsData.csv')

# Create a function which will run all variable selection techniques
# It will then return a binary matrix with slected variables
# and also fit metrics for the 3 classses of models
returnBinMat <- function(dataFrame, grepVal, genderVal, corVal){
  # First run step wise regression
  stepOutput <- returnCVStepFit(dataFrame=dataFrame, grepID=grepVal, genderID=genderVal, nCor=corVal, selectionPercent=.95, iterationCount=100)
  
  # Prepare the data for eNet and RF
  dataFrameS <- dataFrame[which(dataFrame$sex==genderVal),]
  col.index <- grep(grepVal, names(dataFrameS))
  outcomeVal <- scale(dataFrameS$F1_Exec_Comp_Cog_Accuracy)[,1]
  inputVals <- scale(dataFrameS[,col.index])[,1:length(col.index)]
  outcomeVal <- outcomeVal[complete.cases(inputVals)]
  inputVals <- inputVals[complete.cases(inputVals),]
  index <- unlist(createFolds(outcomeVal, k=3, list=T, returnTrain=T)[1])

  # Now run enet
  enetBetas <- runLassoforHiLo(inputVals, outcomeVal, nCor=corVal, alphaSequence=.5)
  enetValues <- rmFat(enetBetas, inputVals, .5)
  enetOutput <- computeModelFitMetrics(returnBetas=T,x = enetValues, y= outcomeVal)

  # Run random forest
  outputRF <- randomForest(x=inputVals[index,], y=outcomeVal[index], xtest=inputVals[-index,],ytest=outcomeVal[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

  # Now create the output binary matrix values
  emptyCol <- matrix(0, nrow=dim(inputVals)[2], ncol=1)
  rownames(emptyCol) <- colnames(inputVals)
  binaryStep <- emptyCol
  binaryStep[rownames(emptyCol) %in% strSplitMatrixReturn(names(stepOutput[[2]])[-1], ')')[,2]] <- 1
  binaryEnet <- emptyCol
  subVal <- grep('cept', names(enetOutput[[2]]))
  binaryEnet[rownames(emptyCol) %in% strSplitMatrixReturn(names(enetOutput[[2]])[-subVal], ')')[,2]] <- 1
  binaryRF <- emptyCol
  binaryRF[rownames(emptyCol) %in% names(outputRF[order(outputRF, decreasing=T)][1:20])] <- 1
  outputBin <- cbind(binaryStep, binaryEnet, binaryRF)
  outputBin <- cbind(outputBin, rowSums(outputBin))
  colnames(outputBin) <- c('Stepwise', 'enet', 'RF', 'RowSums')
  # Now I need to compute the fit metrics for the various levels of inclusion... 1,2,3
  fitStats <- NA
  for(q in seq(1, max(outputBin[,4]))){
    # First find our values to include in the model
    tmpDex <- which(outputBin[,4] >= q)
    x <- inputVals[,tmpDex]
    y <- outcomeVal
    modm <- lm(y ~ as.matrix(x))
    if(!identical(dim(x), NULL)){
      n <- dim(x)[1]
      p <- dim(x)[2]
    }
    if(identical(dim(x), NULL)){
      n <- length(x)
      p <- 1
    }
    # Now do a crossval of our model
    modmCV <- crossval(x, y, theta.fit, theta.predict,ngroup=10)

    # Now get our output metrics 
    rawRSquared <- cor(y, modm$fitted.values)^2
    cvRSquared <- cor(y, modmCV$cv.fit)^2
    rawICC <- ICC(cbind(y, modm$fitted.values))$results[4,2]
    cvICC <- ICC(cbind(y, modmCV$cv.fit))$results[4,2]
    rawRMSE <- sqrt(mean((y-modm$fitted.values)^2))
    cvRMSE <- sqrt(mean((y-modmCV$cv.fit)^2))
    adjRSquared <- cor(y, modm$fitted.values)^2 - 
                (1 - cor(y, modm$fitted.values)^2)*(p/(n-p-1))
    output <- as.data.frame(cbind(n,p,rawRSquared,cvRSquared,rawICC,cvICC,rawRMSE,cvRMSE,adjRSquared))
    colnames(output) <- c('n','p','R2', 'CVR2', 'ICC', 'CVICC', 'RMSE', 'CVRMSE', 'ADJR2')
    fitStats <- rbind(fitStats, output)
  }
  return(list(outputBin=outputBin, fitStats=fitStats))
} 

# Now run this for each modality and gender
genderVals <- c(1,2)
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'fa.data.label')
dataGrepNames <- c('mprage_jlf_vol', 'pcasl_jlf_cbf', 'mprage_jlf_gmd','dti_jlf_tr', 'dti_dtitk_jhulabel_fa')
outputBinMat <- NA
for(g in genderVals){
  for(z in 1:length(dataNames)){
    vals <- returnBinMat(get(dataNames[z]), dataGrepNames[z], g, 30)
    binMat <- vals[[1]]
    fitStats <- vals[[2]]
    binMatName <- paste(strSplitMatrixReturn(dataNames[z], '.data')[1],g,'binaryInclusionMat.csv', sep='')
    outputBinMat <- rbind(outputBinMat, binMat)
    fitStatsName <- paste(strSplitMatrixReturn(dataNames[z], '.data')[1],g,'fitStats.csv', sep='')
    #write.csv(binMat, binMatName, quote=F, row.names=T)
    #write.csv(fitStats, fitStatsName, quote=F, row.names=T)
  }
}

# Now run forward step wise regression for the all data model
tmp <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
tmp <- merge(tmp, gmd.data,  by=intersect(names(tmp), names(gmd.data)))
#tmp <- merge(tmp, ct.data,  by=intersect(names(tmp), names(ct.data)))
#tmp <- merge(tmp, reho.data,  by=intersect(names(tmp), names(reho.data)))
#tmp <- merge(tmp, alff.data,  by=intersect(names(tmp), names(alff.data)))
tmp <- merge(tmp, tr.data,  by=intersect(names(tmp), names(tr.data)))
tmp <- merge(tmp, fa.data.label, by=intersect(names(tmp), names(fa.data.label)))
tmp <- merge(tmp, tr.data.label, by=intersect(names(tmp), names(tr.data.label)))
all.data <- tmp

# Produce our models based on previously selected variables 
outputBinMat <- outputBinMat[-1,]
outputBinMale <- outputBinMat[1:278,]
outputBinFemale <- outputBinMat[279:556,]

## Quickly test SVR here
svr.10fold<-function(y,features){
  nfolds=10
  n<-length(y)
  fits<-rep(NA,n)
  rand.vec<-sample( ceiling(seq(0.0001,(nfolds-0.0001), length.out=n)) )
  for(fold in 1:nfolds){
    y.sub<-y[which(rand.vec!=fold)]
    features.sub<-features[which(rand.vec!=fold),]
    svr.fold<-svm(y=y.sub, x=features.sub, cross=1, kernel='linear')
    fits[which(rand.vec==fold)]<-predict(svr.fold,features[which(rand.vec==fold),])
    cat('Done fold', paste(fold, nfolds, sep='/'), '\n')
  }
  return(fits)
}
tmp <- svm(formula=maleFormula, cross=10, kernal='linear', data=all.data[which(all.data$sex==1),], type='eps-regression')



# Procuce a male model out metrics
maleOut <- NA

maleFormula <- paste(rownames(outputBinMale)[which(outputBinMale[,1]==1)], collapse='+')
maleFormula <- as.formula(paste("F1_Exec_Comp_Cog_Accuracy ~", maleFormula, sep=''))
modStep <- lm(maleFormula, data=all.data[which(all.data$sex==1),])
modStepFit <- returnFitMetricsFromModel(modStep)

# Now do enet
maleFormula <- paste(rownames(outputBinMale)[which(outputBinMale[,2]==1)], collapse='+')
maleFormula <- as.formula(paste("F1_Exec_Comp_Cog_Accuracy ~", maleFormula, sep=''))
modEnet <- lm(maleFormula, data=all.data[which(all.data$sex==1),])
modEnetFit <- returnFitMetricsFromModel(modEnet)
# Now do RF
maleFormula <- paste(rownames(outputBinMale)[which(outputBinMale[,3]==1)], collapse='+')
maleFormula <- as.formula(paste("F1_Exec_Comp_Cog_Accuracy ~", maleFormula, sep=''))
modRF <- lm(maleFormula, data=all.data[which(all.data$sex==1),])
modRFFit <- returnFitMetricsFromModel(modRF)
# Now write the output
maleOut <- rbind(modStepFit, modEnetFit, modRFFit)
write.csv(maleOut, 'maleIndividualModalSelect.csv', quote=F, row.names=F)


femaleFormula <- paste(rownames(outputBinFemale)[which(outputBinFemale[,1]==1)], collapse='+')
femaleFormula <- as.formula(paste("F1_Exec_Comp_Cog_Accuracy ~", femaleFormula, sep=''))
modStep <- lm(femaleFormula, data=all.data[which(all.data$sex==2),])
modStepFit <- returnFitMetricsFromModel(modStep)
# Now do enet
femaleFormula <- paste(rownames(outputBinFemale)[which(outputBinFemale[,2]==1)], collapse='+')
femaleFormula <- as.formula(paste("F1_Exec_Comp_Cog_Accuracy ~", femaleFormula, sep=''))
modEnet <- lm(femaleFormula, data=all.data[which(all.data$sex==2),])
modEnetFit <- returnFitMetricsFromModel(modEnet)
# Now do RF
femaleFormula <- paste(rownames(outputBinFemale)[which(outputBinFemale[,3]==1)], collapse='+')
femaleFormula <- as.formula(paste("F1_Exec_Comp_Cog_Accuracy ~", femaleFormula, sep=''))
modRF <- lm(femaleFormula, data=all.data[which(all.data$sex==2),])
modRFFit <- returnFitMetricsFromModel(modRF)
# Now write the csv
femaleOut <- rbind(modStepFit, modEnetFit, modRFFit)
write.csv(femaleOut, 'femaleIndividualModalSelect.csv', quote=F, row.names=F)


# Produce our binary matrices
colnames(all.data) <- gsub(x=colnames(all.data), pattern='dti_dtitk_jhulabel', replacement='jlf_dti_dtitk_jhulabel')
maleAllValues <- returnBinMat(all.data, 'jlf', 1, 30)
femaleAllValues <- returnBinMat(all.data, 'jlf', 2, 30)

# Now write the output values
write.csv(maleAllValues[[1]], 'maleAllBinMat.csv', quote=F, row.names=T)
write.csv(femaleAllValues[[1]], 'femaleAllBinMat.csv', quote=F, row.names=T)
write.csv(maleAllValues[[2]], 'maleAllFitStat.csv', quote=F, row.names=T)
write.csv(femaleAllValues[[2]], 'femaleAllFitStat.csv', quote=F, row.names=T)
