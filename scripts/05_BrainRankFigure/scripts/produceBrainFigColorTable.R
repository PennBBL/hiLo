# AFGR June 2017
# This script will be used to produce the color tables needed for the JLF beta weight figures

# Load library(s)
source('/home/adrose/hiLo/scripts/05_BrainRankFigure/functions/functions.R')

## Load data
maleData <- read.csv('/home/adrose/hiLo/data/04_CognitiveModels/maleFitBetas.csv')
femaleData <- read.csv('/home/adrose/hiLo/data/04_CognitiveModels/femaleFitBetas.csv')

## Now produce all of the modality & gender specific color tables 
writeColorTableandKey(inputData=maleData, inputColumn=2, outName='volMale')
writeColorTableandKey(inputData=maleData, inputColumn=3, outName='cbfMale')
writeColorTableandKey(inputData=maleData, inputColumn=4, outName='gmdMale')
writeColorTableandKey(inputData=maleData, inputColumn=5, outName='ctMale')
writeColorTableandKey(inputData=maleData, inputColumn=6, outName='rehoMale')
writeColorTableandKey(inputData=maleData, inputColumn=7, outName='alffMale')
writeColorTableandKey(inputData=maleData, inputColumn=8, outName='trMale')

writeColorTableandKey(inputData=femaleData, inputColumn=2, outName='volfeMale')
writeColorTableandKey(inputData=femaleData, inputColumn=3, outName='cbffeMale')
writeColorTableandKey(inputData=femaleData, inputColumn=4, outName='gmdfeMale')
writeColorTableandKey(inputData=femaleData, inputColumn=5, outName='ctfeMale')
writeColorTableandKey(inputData=femaleData, inputColumn=6, outName='rehofeMale')
writeColorTableandKey(inputData=femaleData, inputColumn=7, outName='alfffeMale')
writeColorTableandKey(inputData=femaleData, inputColumn=8, outName='trfeMale')

