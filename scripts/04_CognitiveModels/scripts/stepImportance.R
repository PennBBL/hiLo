# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych','reshape2', 'caret', 'randomForest', 'MASS','SignifReg')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
fa.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabelsData.csv')
tr.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuTRLabelsData.csv')
raceVals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')

# Now loop through all of our modalities, data frames, and genders and prep our output
genderVals <- c(1,2)
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_')

# Now create our loops
for(g in genderVals){
  selectedOutput <- matrix(0, nrow=length(grep('mprage_jlf_vol', names(vol.data))), ncol=4)
  rownames(selectedOutput) <- names(vol.data)[grep('mprage_jlf_vol', names(vol.data))]
  rownames(selectedOutput) <- gsub(x=rownames(selectedOutput), pattern='mprage_jlf_vol_', replacement='')
  colnames(selectedOutput) <- dataNames
  selectedBeta <- matrix(0, nrow=length(grep('mprage_jlf_vol', names(vol.data))), ncol=4)
  rownames(selectedBeta) <- names(vol.data)[grep('mprage_jlf_vol', names(vol.data))]
  rownames(selectedBeta) <- gsub(x=rownames(selectedBeta), pattern='mprage_jlf_vol_', replacement='')
  colnames(selectedBeta) <- dataNames
  allOut <- NA
  for(z in 1:length(dataNames)){
    #tmpDF <- returnPercentileGroup('me', get(dataNames[z])[,grep('F1_Exec_Comp_Cog_Accuracy', names(get(dataNames[z])))], get(dataNames[z]))
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    #tmpDF <- tmpDF[tmpDF$bblid %in% raceVals$bblid[which(raceVals$race2==1)],]
    vals <- returnCVStepFit(dataFrame=tmpDF, grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=100, nCor=5, selectionPercent=0)
    colIndex <- grep(dataGrepNames[z], names(tmpDF))
    colIndex <- append(grep('F1_Exec_Comp_Cog_Accuracy', names(tmpDF)), colIndex)
    inputData <- tmpDF[,colIndex]
    inputData <- regressWithinModality(inputData, grepPattern=dataGrepNames[z])
    modm <- lm(F1_Exec_Comp_Cog_Accuracy~., data=inputData)
    coefVals <- coefficients(modm)[2:length(coefficients(modm))]
    names(coefVals) <- gsub(x=names(coefVals), pattern=dataGrepNames[z], replacement='')
    selectedBeta[match(names(coefVals), rownames(selectedOutput)),z] <- coefVals
    nVals <- vals[[3]]
    rownames(nVals) <- gsub(x=rownames(nVals), pattern=dataGrepNames[z], replacement='')
    selectedOutput[match(rownames(nVals), rownames(nVals)),z] <- nVals[,1]
  }
 nameVal <- paste(g, "selectedBetaVals.csv", sep='')
 write.csv(selectedBeta, nameVal, quote=F)
 nameVal <- paste(g, "selectedNVals.csv", sep='')
 write.csv(selectedOutput, nameVal, quote=F)
}

# Now run variable selection with the modality regressed variables and see if this alters our selection at all
vol.data.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv')
cbf.data.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/cbfData.csv')
gmd.data.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/gmdData.csv')
tr.data.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfTRData.csv')

dataNamesMR <- c('vol.data.mr', 'cbf.data.mr', 'gmd.data.mr', 'tr.data.mr')
# Now create our loops
for(g in genderVals){
  selectedOutput <- matrix(0, nrow=length(grep('mprage_jlf_vol', names(vol.data))), ncol=4)
  rownames(selectedOutput) <- names(vol.data)[grep('mprage_jlf_vol', names(vol.data))]
  rownames(selectedOutput) <- gsub(x=rownames(selectedOutput), pattern='mprage_jlf_vol_', replacement='')
  colnames(selectedOutput) <- dataNames
  selectedBeta <- matrix(0, nrow=length(grep('mprage_jlf_vol', names(vol.data))), ncol=4)
  rownames(selectedBeta) <- names(vol.data)[grep('mprage_jlf_vol', names(vol.data))]
  rownames(selectedBeta) <- gsub(x=rownames(selectedBeta), pattern='mprage_jlf_vol_', replacement='')
  colnames(selectedBeta) <- dataNames
  allOut <- NA
  for(z in 1:length(dataNames)){
    tmpDF <- get(dataNamesMR[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    #tmpDF <- tmpDF[tmpDF$bblid %in% raceVals$bblid[which(raceVals$race2==1)],]
    vals <- returnCVStepFit(dataFrame=tmpDF, grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=100, nCor=10, selectionPercent=0, regressWithin=FALSE)
    colIndex <- grep(dataGrepNames[z], names(tmpDF))
    colIndex <- append(grep('F1_Exec_Comp_Cog_Accuracy', names(tmpDF)), colIndex)
    inputData <- scale(tmpDF[,colIndex])
    modm <- bootStrapBetaWeight(y=inputData[,1], x=inputData[,2:length(colIndex)])
    coefVals <- coefficients(modm)[2:length(coefficients(modm))]
    names(coefVals) <- gsub(x=names(coefVals), pattern=dataGrepNames[z], replacement='')
    nVals <- vals[[3]]
    rownames(nVals) <- gsub(x=rownames(nVals), pattern=dataGrepNames[z], replacement='')
    selectedBeta[match(names(coefVals), rownames(selectedOutput)),z] <- coefVals
    selectedOutput[match(rownames(nVals), rownames(selectedOutput)),z] <- nVals[,1]
  }
 nameVal <- paste(g, "selectedBetaValsMR.csv", sep='')
 write.csv(selectedBeta, nameVal, quote=F)
 nameVal <- paste(g, "selectedNValsMR.csv", sep='')
 write.csv(selectedOutput, nameVal, quote=F)
}

# Now combine the data sets 
namesNon <- c('selectedBetaVals', 'selectedNVals')
for(g in genderVals){
  nameVal <- paste(g, namesNon[1], '.csv', sep='')
  nameVal2 <- paste(g, namesNon[1], 'MR.csv', sep='')
  valsNon <- read.csv(nameVal)
  valsMr <- read.csv(nameVal2)
  colnames(valsNon)[2:dim(valsNon)[2]] <- paste(colnames(valsNon[2:dim(valsNon)[2]]), '.RAW', sep='')
  colnames(valsMr)[2:dim(valsMr)[2]] <- paste(colnames(valsNon[2:dim(valsMr)[2]]), '.MR', sep='')
  output <- merge(valsNon, valsMr, by=intersect(names(valsNon), names(valsMr)))
  outName <- paste(g, 'OutBetaVals.csv', sep='')
  output <- cbind(output, apply(output[,2:9], 2, rank))  
  write.csv(output, outName, quote=F, row.names=F)

  # Now do the same for the selection N's
  nameVal <- paste(g, namesNon[2], '.csv', sep='')
  nameVal2 <- paste(g, namesNon[2], 'MR.csv', sep='')
  valsNon <- read.csv(nameVal)
  valsMr <- read.csv(nameVal2)
  colnames(valsNon)[2:dim(valsNon)[2]] <- paste(colnames(valsNon[2:dim(valsNon)[2]]), '.RAW', sep='')
  colnames(valsMr)[2:dim(valsMr)[2]] <- paste(colnames(valsNon[2:dim(valsMr)[2]]), '.MR', sep='')
  output <- merge(valsNon, valsMr, by=intersect(names(valsNon), names(valsMr)))
  outName <- paste(g, 'OutSelectVals.csv', sep='')
  output <- cbind(output, apply(output[,2:9], 2, rank))
  write.csv(output, outName, quote=F, row.names=F)  
}
