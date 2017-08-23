# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'MASS','SignifReg')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
#vol.data <- vol.data[,-grep("4th_Ventricle", names(vol.data))]
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
    vals <- returnCVStepFit(dataFrame=tmpDF, grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=100, nCor=30, selectionPercent=0)
    tmp2 <- vals[[2]][2:length(vals[[2]])]
    names(tmp2) <- strSplitMatrixReturn(names(tmp2), dataGrepNames[z])[,2]
    colIndex <- grep(dataGrepNames[z], names(get(dataNames[z])))
    colIndex <- append(grep('F1_Exec_Comp_Cog_Accuracy', names(get(dataNames[z]))), colIndex)
    stepVAR <- SignifReg(scope=F1_Exec_Comp_Cog_Accuracy~., data=get(dataNames[z])[which(get(dataNames[z])$sex==g),colIndex], alpha=.05, direction="forward", criterion="p-value")
    foobar <- stepVAR$coefficients[2:length(stepVAR$coefficients)]
    newColIndex <- match(names(foobar), names(get(dataNames[z])))
    newColIndex <- append(grep('F1_Exec_Comp_Cog_Accuracy', names(get(dataNames[z]))), newColIndex)
    inputData <- scale(tmpDF[which(get(dataNames[z])$sex==g),newColIndex])
    modelOut <- as.formula(paste('F1_Exec_Comp_Cog_Accuracy ~', paste(names(foobar), collapse='+')))
    if(dim(inputData)[2] > 2){
      inputData <- regressWithinModality(inputData, grepPattern=dataGrepNames[z])
    }
    modm <- lm(modelOut, data=as.data.frame(inputData))
    foobarlar <- coefficients(modm)[2:length(coefficients(modm))]
    names(foobarlar) <- gsub(x=names(foobarlar), pattern=dataGrepNames[z], replacement='')
    foobar <- vals[[2]][2:length(vals[[2]])]
    names(foobar) <- strSplitMatrixReturn(gsub(x=names(foobar), pattern=dataGrepNames[z], replacement=''), ')')[,2]
    tmp <- cbind(foobarlar, foobar[match(names(foobarlar), names(foobar))])
    print(cor(tmp[,1], tmp[,2]))
    selectedBeta[match(names(foobar), rownames(selectedOutput)),z] <- foobar
    foobar <- vals[[3]]
    rownames(foobar) <- gsub(x=rownames(foobar), pattern=dataGrepNames[z], replacement='')
    selectedOutput[match(rownames(foobar), rownames(selectedOutput)),z] <- foobar[,1]
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
    #tmpDF <- returnPercentileGroup('me', get(dataNames[z])[,grep('F1_Exec_Comp_Cog_Accuracy', names(get(dataNames[z])))], get(dataNames[z]))
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    vals <- returnCVStepFit(dataFrame=tmpDF, grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=100, nCor=30, selectionPercent=0, regressWithin=FALSE)
    tmp2 <- vals[[2]][2:length(vals[[2]])]
    names(tmp2) <- strSplitMatrixReturn(names(tmp2), dataGrepNames[z])[,2]
    colIndex <- grep(dataGrepNames[z], names(get(dataNames[z])))
    colIndex <- append(grep('F1_Exec_Comp_Cog_Accuracy', names(get(dataNames[z]))), colIndex)
    stepVAR <- SignifReg(scope=F1_Exec_Comp_Cog_Accuracy~., data=get(dataNames[z])[which(get(dataNames[z])$sex==g),colIndex], alpha=.05, direction="forward", criterion="p-value")
    foobar <- stepVAR$coefficients[2:length(stepVAR$coefficients)]
    newColIndex <- match(names(foobar), names(get(dataNames[z])))
    newColIndex <- append(grep('F1_Exec_Comp_Cog_Accuracy', names(get(dataNames[z]))), newColIndex)
    inputData <- scale(tmpDF[,newColIndex])
    modelOut <- as.formula(paste('F1_Exec_Comp_Cog_Accuracy ~', paste(names(foobar), collapse='+')))
    if(dim(inputData)[2] > 2){
      inputData <- regressWithinModality(inputData, grepPattern=dataGrepNames[z])
    }
    modm <- lm(modelOut, data=as.data.frame(inputData))
    foobarlar <- coefficients(modm)[2:length(coefficients(modm))]
    names(foobarlar) <- gsub(x=names(foobarlar), pattern=dataGrepNames[z], replacement='')
    foobar <- vals[[2]][2:length(vals[[2]])]
    names(foobar) <- strSplitMatrixReturn(gsub(x=names(foobar), pattern=dataGrepNames[z], replacement=''), ')')[,2]
    tmp <- cbind(foobarlar, foobar[match(names(foobarlar), names(foobar))])
    print(cor(tmp[,1], tmp[,2], use='complete'))
    selectedBeta[match(names(foobar), rownames(selectedOutput)),z] <- foobar
    foobar <- vals[[3]]
    rownames(foobar) <- gsub(x=rownames(foobar), pattern=dataGrepNames[z], replacement='')
    selectedOutput[match(rownames(foobar), rownames(selectedOutput)),z] <- foobar[,1]
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
  write.csv(output, outName, quote=F, row.names=F)  
}
