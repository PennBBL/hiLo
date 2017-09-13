# AFGR June 2017
# This script will be used to produce the color tables needed for the JLF beta weight figures

# Load library(s)
source('/home/adrose/hiLo/scripts/05_BrainRankFigure/functions/functions.R')

## Load data
maleDataVol <- read.csv('output_hi-me-low_vol_male.csv', row.names=1)
maleDataVol[,1] <- strSplitMatrixReturn(maleDataVol[,1], 'mprage_jlf_vol_')[,2]
maleDataGmd <- read.csv('output_hi-me-low_gmd_male.csv', row.names=1)
maleDataGmd[,1] <- strSplitMatrixReturn(maleDataGmd[,1], 'mprage_jlf_gmd_')[,2]
maleDataCbf <- read.csv('output_hi-me-low_cbf_male.csv', row.names=1)
maleDataCbf[,1] <- strSplitMatrixReturn(maleDataCbf[,1], 'pcasl_jlf_cbf_')[,2]
femaleDataVol <- read.csv('output_hi-me-low_vol_female.csv', row.names=1)
femaleDataVol[,1] <- strSplitMatrixReturn(femaleDataVol[,1], 'mprage_jlf_vol_')[,2]
femaleDataGmd <- read.csv('output_hi-me-low_gmd_female.csv', row.names=1)
femaleDataGmd[,1] <- strSplitMatrixReturn(femaleDataGmd[,1], 'mprage_jlf_gmd_')[,2]
femaleDataCbf <- read.csv('output_hi-me-low_cbf_female.csv', row.names=1)
femaleDataCbf[,1] <- strSplitMatrixReturn(femaleDataCbf[,1], 'pcasl_jlf_cbf_')[,2]
maleDataTr <- read.csv('output_hi-me-low_tr_male.csv', row.names=1)
maleDataTr[,1] <-  strSplitMatrixReturn(maleDataTr[,1], 'dti_jlf_tr_')[,2]
femaleDataTr <- read.csv('output_hi-me-low_tr_female.csv', row.names=1)
femaleDataTr[,1] <-  strSplitMatrixReturn(femaleDataTr[,1], 'dti_jlf_tr_')[,2]



## Now produce all of the modality & gender specific color tables 
writeColorTableandKey(inputData=maleDataVol, inputColumn=2, outName='volMaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=maleDataGmd, inputColumn=2, outName='gmdMaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=maleDataCbf, inputColumn=2, outName='cbfMaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=maleDataTr, inputColumn=2, outName='trMaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataVol, inputColumn=2, outName='volFemaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataGmd, inputColumn=2, outName='gmdFemaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataCbf, inputColumn=2, outName='cbfFemaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataTr, inputColumn=2, outName='trFemaleLo', minTmp=c(-.8, 0), maxTmp=c(0, .8))


writeColorTableandKey(inputData=maleDataVol, inputColumn=3, outName='volMaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=maleDataGmd, inputColumn=3, outName='gmdMaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=maleDataCbf, inputColumn=3, outName='cbfMaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=maleDataTr, inputColumn=3, outName='trMaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataVol, inputColumn=3, outName='volFemaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataGmd, inputColumn=3, outName='gmdFemaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataCbf, inputColumn=3, outName='cbfFemaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
writeColorTableandKey(inputData=femaleDataTr, inputColumn=3, outName='trFemaleHi', minTmp=c(-.8, 0), maxTmp=c(0, .8))
