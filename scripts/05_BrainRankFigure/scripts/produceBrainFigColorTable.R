# AFGR June 2017
# This script will be used to produce the color tables needed for the JLF beta weight figures

# Load library(s)
source('/home/adrose/hiLo/scripts/05_BrainRankFigure/functions/functions.R')

## Load data
maleDataVol <- read.csv('/home/angelgar/highMedLow/output_hi-me-low_vol_male.csv')
maleDataGmd <- read.csv('/home/angelgar/highMedLow/output_hi-me-low_gmd_male.csv')
maleDataCbf <- read.csv('/home/angelgar/highMedLow/output_hi-me-low_cbf_male.csv')
femaleDataVol <- read.csv('/home/angelgar/highMedLow/output_hi-me-low_vol_female.csv')
femaleDataGmd <- read.csv('/home/angelgar/highMedLow/output_hi-me-low_gmd_female.csv')
femaleDataCbf <- read.csv('/home/angelgar/highMedLow/output_hi-me-low_cbf_female.csv')

## Now produce all of the modality & gender specific color tables 
writeColorTableandKey(inputData=maleDataVol, inputColumn=5, outName='volMale')
writeColorTableandKey(inputData=maleDataGmd, inputColumn=5, outName='gmdMale')
writeColorTableandKey(inputData=maleDataCbf, inputColumn=5, outName='cbfMale')
writeColorTableandKey(inputData=femaleDataVol, inputColumn=5, outName='volFemale')
writeColorTableandKey(inputData=femaleDataGmd, inputColumn=5, outName='gmdFemale')
writeColorTableandKey(inputData=femaleDataCbf, inputColumn=5, outName='cbfFemale')
