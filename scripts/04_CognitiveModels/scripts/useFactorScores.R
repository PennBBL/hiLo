source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')


# Load the data 
factorScores <- read.csv('/home/adrose/Brain_Factor_Scores_Volume_GMD_Cortical-Thickness.csv')
factorScoresFunc <- read.csv('/home/adrose/brain_factor_scores_reho_alff.csv')
#factorScores <- merge(factorScores, factorScoresFunc)
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
allDat <- merge(factorScores, vol.data)
allDatF <- merge(factorScoresFunc, vol.data)

# Now I need to produce the CV r-squared using ridge regression for every modalities factor scores

dataGrepNames <- c('Volume_', 'Cortical_Thickness_', 'ReHo_', 'ALFF_')
dataNames <- c('allDat', 'allDat', 'allDatF', 'allDatF')
allRVals <- NULL
for(emptyVal in 1:10){
for(g in 1:2){
  for(z in 1:length(dataGrepNames)){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    print(dim(tmp.values))
    # Now produce the beta weights
    outVals <- runRidgeOnAll(x=tmp.values, y=tmp.out, rmLmSummary=F)
    # Now get all of the R^2 values
    eNet <- cor(outVals[[1]], tmp.out)^2
    linReg <- cor(outVals[[2]], tmp.out)^2
    out <- cbind(g,dataGrepNames[z], eNet, linReg)
    allRVals <- rbind(allRVals, out)
  }
}
}
allRVals <- as.data.frame(allRVals)
# Now try this with running the tpot paradigm 
outPlot <- ggplot(allRVals, aes(x=V2, fill=factor(g), y=as.numeric(as.character(eNet)))) + 
  geom_violin()
