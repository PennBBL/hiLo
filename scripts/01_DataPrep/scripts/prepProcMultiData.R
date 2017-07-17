# AFGR June 2017
# This script will be used to prepare the input CSV files for the proc multi sas scripts
# These scripts will be looing at 4 way interactions and 3 way interactions. The interactions include:
# age_bin*sex*roi*perf_bin
# sex*perf_bin*age_bin
# The input to this script will be the averaged L and R ROI values for all modalities

# load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('reshape2')

## Load data here
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

## Now attach age bin to all of our data 
vol.data <- addAgeBin(vol.data, vol.data$ageAtGo1Scan, 167, 215, 216)
cbf.data <- addAgeBin(cbf.data, cbf.data$ageAtGo1Scan, 167, 215, 216)
gmd.data <- addAgeBin(gmd.data, gmd.data$ageAtGo1Scan, 167, 215, 216)
ct.data <- addAgeBin(ct.data, ct.data$ageAtGo1Scan, 167, 215, 216)
reho.data <- addAgeBin(reho.data, reho.data$ageAtGo1Scan, 167, 215, 216)
alff.data <- addAgeBin(alff.data, alff.data$ageAtGo1Scan, 167, 215, 216)
ad.data <- addAgeBin(ad.data, ad.data$ageAtGo1Scan, 167, 215, 216)
fa.data <- addAgeBin(fa.data, fa.data$ageAtGo1Scan, 167, 215, 216)
rd.data <- addAgeBin(rd.data, rd.data$ageAtGo1Scan, 167, 215, 216)
tr.data <- addAgeBin(tr.data, tr.data$ageAtGo1Scan, 167, 215, 216)

# Now create a function to do everything in one call 
doEverythingWide <- function(dataFrame, grepPattern){
  tmp1 <- outputLongFormat4way(dataFrame=dataFrame, modalityName=grepPattern, ageBand='Childhood')
  tmp2 <- outputLongFormat4way(dataFrame=dataFrame, modalityName=grepPattern, ageBand='Adolescence')
  tmp3 <- outputLongFormat4way(dataFrame=dataFrame, modalityName=grepPattern, ageBand='Early Adulthood')
  allData <- rbind(tmp1, tmp2, tmp3)

  # Now rm ROI's that do not belong to lobes 1-9
  allData <- allData[which(allData$lobe!=10),]

  # Now ensure we only return complete cases
  allData <- allData[complete.cases(allData),]

  # Now return the data
  return(allData)
}

# Now run through errything 
datFrame <- c('vol.data', 'cbf.data', 'gmd.data', 'ct.data', 'reho.data', 'alff.data', 'tr.data')
colNames <- c('mprage_jlf_vol', 'pcasl_jlf_cbf', 'mprage_jlf_gmd', 'mprage_jlf_ct', 'rest_jlf_reho', 'rest_jlf_alff', 'dti_jlf_tr')
basePath <- '/home/adrose/dataPrepForHiLoPaper/data/longDataForProcMixed/'
for(i in 1:length(datFrame)){
  dataF <- datFrame[i]
  colName <- colNames[i]
  modalName <- strSplitMatrixReturn(colName, '_')[,3]
  outPath <- paste(basePath,modalName,'-LongDataFrame.csv', sep='')
  tmp <- doEverythingWide(get(dataF), colName)
  write.csv(tmp, outPath, quote=F, row.names=F) 
}
