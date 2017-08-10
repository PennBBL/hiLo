# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'MASS','SignifReg')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
vol.data <- vol.data[,-grep("4th_Ventricle", names(vol.data))]
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

# Now loop through all of our modalities, data frames, and genders and prep our output
genderVals <- c(1,2)
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_')

# Now create our loops
selectedOutput <- matrix(0, nrow=length(grep('mprage_jlf_vol', names(vol.data))), ncol=4)
rownames(selectedOutput) <- names(vol.data)[grep('mprage_jlf_vol', names(vol.data))]
rownames(selectedOutput) <- gsub(x=rownames(selectedOutput), pattern='mprage_jlf_vol_', replacement='')
colnames(selectedOutput) <- dataNames
selectedBeta <- matrix(0, nrow=length(grep('mprage_jlf_vol', names(vol.data))), ncol=4)
rownames(selectedBeta) <- names(vol.data)[grep('mprage_jlf_vol', names(vol.data))]
rownames(selectedBeta) <- gsub(x=rownames(selectedBeta), pattern='mprage_jlf_vol_', replacement='')
colnames(selectedBeta) <- dataNames
allOut <- NA
for(w in seq(.95, .95, .05)){
  for(g in genderVals){
    for(z in 1:length(dataNames)){
      vals <- returnCVStepFit(dataFrame=get(dataNames[z]), grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=100, nCor=30, selectionPercent=w)
      tmp2 <- vals[[2]][2:length(vals[[2]])]
      names(tmp2) <- strSplitMatrixReturn(names(tmp2), dataGrepNames[z])[,2]
      selectedBeta[match(names(tmp2), rownames(selectedBeta))] <- tmp2
      foobar <- vals[[3]]
      rownames(foobar) <- gsub(x=rownames(foobar), pattern=dataGrepNames[z], replacement='')
      selectedOutput[match(rownames(foobar), rownames(selectedOutput)),z] <- foobar[,1]
    }
   nameVal <- paste(g, "selectedBetaVals.csv", sep='')
   write.csv(selectedBeta, nameVal, quote=F)
   nameVal <- paste(g, "selectedNVals.csv", sep='')
   write.csv(selectedOutput, nameVal, quote=F)
  }
}
