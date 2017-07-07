# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'MASS')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
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

# Create a function to run step wise
# and return the fit metrics w/in the training and a CV sample
returnCVStepFit <- function(dataFrame, genderID, grepID){
  # Prepare our data
  isolatedGender <- dataFrame[which(dataFrame$sex==genderID),]
  colsOfInterest <- grep(grepID, names(isolatedGender))
  valuesToUse <- scale(isolatedGender[,colsOfInterest])[,1:length(colsOfInterest)]
  outcomeVal <- scale(isolatedGender$F1_Exec_Comp_Cog_Accuracy)
  dataToUse <- as.data.frame(cbind(outcomeVal, valuesToUse))
  outcomeVal <- outcomeVal[complete.cases(valuesToUse)]
  valuesToUse <- valuesToUse[complete.cases(valuesToUse),]

  # Now create a CV sample
  folds <- createFolds(dataToUse$V1, k=10, list=T, returnTrain=T)
  index <- unlist(folds[1])
  trainData <- dataToUse[index,]
  testData <- dataToUse[-index,]

  # Now prepare our models
  nullModel <- lm(V1 ~ 1, data=trainData)
  fullModel <- lm(V1 ~ ., data=trainData)
  stepVAR <- stepAIC(fullModel, direction="both")
  # Now get our model
  modelOut <- as.formula(paste('V1 ~', paste(colnames(stepVAR$model)[2:dim(stepVAR$model)[2]], collapse='+')))
  # Now produce our final models
  modelToTest <- lm(modelOut, data=trainData)
  cvValues <- predict(modelToTest, newdata=testData)

  # Grab our n and p values
  n <- dim(trainData)[1]
  p <- length(colnames(stepVAR$model)[2:dim(stepVAR$model)[2]])

  # Now create our fit metrics
  rawRSquared <- cor(trainData$V1, modelToTest$fitted.values)^2
  cvRSquared <- cor(cvValues, testData$V1)^2
  rawICC <- ICC(cbind(trainData$V1, modelToTest$fitted.values))$results[4,2]
  cvICC <- ICC(cbind(cvValues, testData$V1))$results[4,2]
  rawRMSE <- sqrt(mean((trainData$V1-modelToTest$fitted.values)^2))
  cvRMSE <- sqrt(mean((cvValues - testData$V1)^2))
  adjRSquared <- cor(trainData$V1, modelToTest$fitted.values)^2 - 
    (1 - cor(trainData$V1, modelToTest$fitted.values)^2)*(p/(n-p-1))

  # Now prepare our output
  output <- as.data.frame(cbind(n,p,rawRSquared,cvRSquared,rawICC,cvICC,rawRMSE,cvRMSE,adjRSquared))
  colnames(output) <- c('n','p','R2', 'CVR2', 'ICC', 'CVICC', 'RMSE', 'CVRMSE', 'ADJR2')
  return(output)  
}

# Now loop through all of our modalities, data frames, and genders and prep our output
genderVals <- c(1,2)
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'ct.data', 'reho.data', 'alff.data', 'tr.data', 'fa.data.label')
dataGrepNames <- c('mprage_jlf_vol', 'pcasl_jlf_cbf', 'mprage_jlf_gmd', 'mprage_jlf_ct', 'rest_jlf_reho', 'rest_jlf_alff','dti_jlf_tr', 'dti_dtitk_jhulabel_fa')

# Now create our loops
allOut <- NA
for(g in genderVals){
  for(z in 1:length(dataNames)){
    vals <- returnCVStepFit(get(dataNames[z]), g, dataGrepNames[z])
    tmp <- cbind(g, dataNames[z], dataGrepNames[z], vals)
    allOut <- rbind(allOut, tmp)
  }
}
write.csv(allOut, 'stepIndividualModal.csv', quote=F, row.names=F)
# Now run forward step wise regression for the all data model
tmp <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
tmp <- merge(tmp, gmd.data,  by=intersect(names(tmp), names(gmd.data)))
tmp <- merge(tmp, ct.data,  by=intersect(names(tmp), names(ct.data)))
#tmp <- merge(tmp, reho.data,  by=intersect(names(tmp), names(reho.data)))
#tmp <- merge(tmp, alff.data,  by=intersect(names(tmp), names(alff.data)))
tmp <- merge(tmp, tr.data,  by=intersect(names(tmp), names(tr.data)))
tmp <- merge(tmp, fa.data.label, by=intersect(names(tmp), names(fa.data.label)))
tmp <- merge(tmp, tr.data.label, by=intersect(names(tmp), names(tr.data.label)))
all.data <- tmp
colnames(all.data) <- gsub(x=colnames(all.data), pattern='dti_dtitk_jhulabel', replacement='jlf_dti_dtitk_jhulabel_fa')

maleAll <- returnCVStepFit(all.data, 1, 'jlf')
femaleAll <-  returnCVStepFit(all.data, 2, 'jlf')
allMOut <- rbind(maleALl, femaleAll)
write.csv(allMOut, 'stepAllModal.csv', quote=F, row.names=F)
