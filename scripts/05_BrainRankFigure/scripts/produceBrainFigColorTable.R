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

## Now produce all of the modality & gender specific color tables 
writeColorTableandKey(inputData=maleDataVol, inputColumn=2, outName='volMaleLo', minTmp=c(range(rbind(femaleDataVol[,2], maleDataVol[,2]))[1], 0), maxTmp=c(0, range(rbind(femaleDataVol[,2], maleDataVol[,2]))[2]))
writeColorTableandKey(inputData=maleDataGmd, inputColumn=2, outName='gmdMaleLo', minTmp=c(range(rbind(femaleDataGmd[,2], maleDataGmd[,2]))[1], 0), maxTmp=c(0, range(rbind(femaleDataGmd[,2], maleDataGmd[,2]))[2]))
writeColorTableandKey(inputData=maleDataCbf, inputColumn=2, outName='cbfMaleLo', minTmp=c(range(rbind(femaleDataCbf[,2], maleDataCbf[,2]))[1], 0), maxTmp=c(0, range(rbind(femaleDataCbf[,2], maleDataCbf[,2]))[2]))
writeColorTableandKey(inputData=femaleDataVol, inputColumn=2, outName='volFemaleLo', minTmp=c(range(rbind(femaleDataVol[,2], maleDataVol[,2]))[1], 0), maxTmp=c(0, range(rbind(femaleDataVol[,2], maleDataVol[,2]))[2]))
writeColorTableandKey(inputData=femaleDataGmd, inputColumn=2, outName='gmdFemaleLo', minTmp=c(range(rbind(femaleDataGmd[,2], maleDataGmd[,2]))[1], 0), maxTmp=c(0, range(rbind(femaleDataGmd[,2], maleDataGmd[,2]))[2]))
writeColorTableandKey(inputData=femaleDataCbf, inputColumn=2, outName='cbfFemaleLo', minTmp=c(range(rbind(femaleDataCbf[,2], maleDataCbf[,2]))[1], 0), maxTmp=c(0, range(rbind(femaleDataCbf[,2], maleDataCbf[,2]))[2]))

writeColorTableandKey(inputData=maleDataVol, inputColumn=3, outName='volMaleHi', minTmp=c(range(rbind(femaleDataVol[,3], maleDataVol[,3]))[1], 0), maxTmp=c(0, range(rbind(femaleDataVol[,3], maleDataVol[,3]))[2]))
writeColorTableandKey(inputData=maleDataGmd, inputColumn=3, outName='gmdMaleHi')
writeColorTableandKey(inputData=maleDataCbf, inputColumn=3, outName='cbfMaleHi')
writeColorTableandKey(inputData=femaleDataVol, inputColumn=3, outName='volFemaleHi', minTmp=c(range(rbind(femaleDataVol[,3], maleDataVol[,3]))[1], 0), maxTmp=c(0, range(rbind(femaleDataVol[,3], maleDataVol[,3]))[2]))
writeColorTableandKey(inputData=femaleDataGmd, inputColumn=3, outName='gmdFemaleHi')
writeColorTableandKey(inputData=femaleDataCbf, inputColumn=3, outName='cbfFemaleHi')
