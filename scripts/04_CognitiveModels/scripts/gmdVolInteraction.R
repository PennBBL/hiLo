# AFGR 2017 June 2017

# This script is going to be used to look for the optimal relationship between volume and density
# This will be assessed using a linear regression and weighted means to find peak R2 for each ROI


## Load library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')

## Load data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
nameVals <- names(vol.data)[names(vol.data) %in% names(gmd.data)]
allData <- merge(gmd.data, vol.data, by=nameVals)

tmpSeq <- seq(.01, .99, .01)
tmpSeqInv <- seq(.99, .01, -.01)
weightValues <- cbind(tmpSeq, tmpSeqInv)

# Get the rois that we are going to loop thorugh 
valsToLoop <- names(allData)[31:91]
valsToLoop <- gsub(valsToLoop, pattern='mprage_jlf_gmd_', replacement='')

# Now loop thorugh each of the values and get the column values from the all data csv
paste1Val <- 'mprage_jlf_gmd_'
paste2Val <- 'mprage_jlf_vol_'
outcome <- allData$F1_Exec_Comp_Cog_Accuracy
outVal <- NULL
for(nameVal in valsToLoop){
  grepVal1 <- paste(paste1Val, nameVal, sep='')
  grepVal2 <- paste(paste2Val, nameVal, sep='')
  colVal1 <- grep(grepVal1, names(allData))[1]
  colVal2 <- grep(grepVal2, names(allData))[1]
  tmpVals <- NULL
  for(wVals in 1:length(tmpSeq)){
    tmpData <- scale(allData[,c(colVal1,colVal2)])
    newVals <- apply(tmpData, 1, function(x) weighted.mean(x, w=weightValues[wVals,]))
    mod1 <- lm(outcome ~ newVals)
    sMod1 <- summary(mod1)$r.squared
    print(paste('GMD weight=', weightValues[wVals,1], 'R^2 value = ', sMod1, ' for ', nameVal, sep=' '))
    tmpVals <- append(tmpVals, sMod1)
  }
  outTmp <- append(nameVal, tmpVals)
  outVal <- rbind(outVal, outTmp)
}

# Because we see just about 0 variance in the R2 across the different weights I am going to explore the 
# package Warren recomended, the "mfp" package
install_load('mfp')
# Now loop thorugh each of the values and get the column values from the all data csv
paste1Val <- 'mprage_jlf_gmd_'
paste2Val <- 'mprage_jlf_vol_'
outcome <- allData$F1_Exec_Comp_Cog_Accuracy
outVal <- NULL
for(nameVal in valsToLoop){
  grepVal1 <- paste(paste1Val, nameVal, sep='')
  grepVal2 <- paste(paste2Val, nameVal, sep='')
  formulaValue1 <- as.formula(paste('F1_Exec_Comp_Cog_Accuracy ~ ', grepVal1, '*', grepVal2)) 
  formulaValue2 <- as.formula(paste('F1_Exec_Comp_Cog_Accuracy ~ ', 'fp(',grepVal1, ',df=4, scale=T)', '*', 'fp(', grepVal2, ',df=4, scale=T)'))
  mod1 <- lm(formulaValue1, data=allData)
  mod2 <- mfp(formulaValue, data=allData, family="gaussian")
}
