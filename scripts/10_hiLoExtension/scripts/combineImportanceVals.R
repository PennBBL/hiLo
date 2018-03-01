# AFGR Feb 2018
# This script will be used to harmonize various "importance metrics" from differenct methods
# These include:
#	1. modal regressed values
#	2. modal regressed value ranks
#	3. hi-lo effect size
#	4. Lasso selection %

## Load library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/10_hiLoExtension/functions/functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

## Now load all of our varibales
modalBetaVals <- read.csv("allFactorBetaValues.csv")
modalBetaRank <- read.csv("allFactorValuesBetaRank.csv")
esValues <- read.csv("outputHiLoEffectsVolume.csv")

## Now work with only male volume values here. 
esValues$ROI <- gsub(x=esValues$ROI, pattern="mprage_jlf_vol_", replacement="")
esValues <- esValues[which(esValues$sex=="M"),]

colIndex <- grep("vol.data.Male", colnames(modalBetaVals))
modalBetaVals <- modalBetaVals[,c(1, colIndex)]

maleVals <- merge(modalBetaRank, esValues, by.x='X', by.y='ROI')
maleVals$zScoreDifferenceF1_Exec_Comp_Cog_Accuracy <- rank(maleVals$zScoreDifferenceF1_Exec_Comp_Cog_Accuracy)

## Now plot exec comp cog accuracy relationships
corVal <- corVal <- paste("r = ", round(cor(maleVals$zScoreDifferenceF1_Exec_Comp_Cog_Accuracy, maleVals$vol.data.Male.F1_Exec_Comp_Cog_Accuracy), digits=2))
plot1 <- ggplot(maleVals, aes(x=zScoreDifferenceF1_Exec_Comp_Cog_Accuracy, y=vol.data.Male.F1_Exec_Comp_Cog_Accuracy, label=X)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method=lm) +
  geom_vline(xintercept = 0 , linetype=3) + 
  geom_abline(intercept=0, slope=1) +
  geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))

pdf("F1ExecHiLoESandModalReg.pdf", height=20, width=20)
plot1
dev.off()
