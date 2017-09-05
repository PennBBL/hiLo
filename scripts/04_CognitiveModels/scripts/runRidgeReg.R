source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods')


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

## This script is going to be used to build ridge regression models in a cv'ed manner
## The important aspect is the twice 10 fold cross validation.
## Models will be trained within the training 9 folds in a 10 -fold cv manner, and then validated in the left out
## 1/10th fold - 10 times.

## I am going to try and build a function which wil do this across each of the folds
### Steps involved in this function include:
###	1.) Creaeting 10 folds to train w/in (caret: createFolds)
###	2.) Train a cv.glmnet w/in the training fold **Tune the lmabda using cv.glmnet**
###		Then train the model using glmnet
###	3.) Get the fit statistics in the left out data set - this is what we return
###	4.) Run 1-3 for each fold - find the average of the fit stats 
### This function should return a crossvallidated fit value for each of the input data set 
runRidgeOnAll <- function(x, y, nFold=10, nCor=5, lambdaSeq=10^seq(3, -2, by = -.1)){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  outputCvBeta <- matrix(0, nrow=dim(x)[2], ncol=10)
  outputCvValsL <- rep(NA, length(y))  
  trainSeq <- seq(1, length(y))
  # Now we need to loop thorugh each fold and get our output fit stats
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- as.matrix(x)[index,]
    trainY <- as.vector(y)[index]
    testX <- as.matrix(x)[-index,]
    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaSeq, nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, testX)
    outputCvBeta[,i] <- coef(modelFit)[-1]

    # Now do the same with linear regression
    tmpDF <- as.data.frame(cbind(trainY, trainX))
    lmMod <- lm(trainY~., data=tmpDF)
    # Now get the predictide values
    outputCvValsL[trainSeq[-index]] <- predict(lmMod, newdata=as.data.frame(testX))
  }

  # Now return the output CvVals
  output <- list()
  output[[1]] <- outputCvValsR
  output[[2]] <- outputCvValsL
  output[[3]] <- outputCvBeta
  return(output)
}

## Now create a loop to go though each image modality, gender, and return the CVR^2
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_')
allRVals <- NA
for(g in 1:2){
  for(z in 1:length(dataNames)){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    print(dim(tmp.values))
    # Now produce the beta weights
    outVals <- runRidgeOnAll(x=tmp.values, y=tmp.out)
    # Now get all of the R^2 values
    eNet <- cor(outVals[[1]], tmp.out)^2
    linReg <- cor(outVals[[2]], tmp.out)^2
    out <- cbind(g,dataNames[z], eNet, linReg)
    allRVals <- rbind(allRVals, out)
  }
}
write.csv(allRVals, 'enetCompareLinReg.csv', quote=F, row.names=F)
