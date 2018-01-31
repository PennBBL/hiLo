## Load library(s)
source('/home/adrose/hiLo/scripts/05_BrainRankFigure/functions/functions.R')

## Load data
all.dat <- read.csv('../../04_CognitiveModels/scripts/F1_Exec_Comp_Cog_AccuracyBetaWeights.csv')

## Now produce each modalities beta weight color table
maxVal <- 4.8
minVal <- -2
for(i in 2:13){
  # First isolate our values
  toMake <- all.dat[,c(1, i)]
  # Now grab the col name
  outputFile <- colnames(all.dat)[i]
  # Now produce the color table
  writeColorTableandKey(inputData=toMake, inputColumn=2, outName=outputFile, minTmp=c(minVal, 0), maxTmp=c(0, maxVal))
}
