source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods')


# Now run variable selection with the modality regressed variables and see if this alters our selection at all
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
tr.data$dti_jlf_tr_MeanTR <- apply(tr.data[,grep('dti_jlf_tr_', names(tr.data))], 1, mean)

# Now prepare the all data values 
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))

## This script is going to be used to build ridge regression models in a cv'ed manner
## The important aspect is the twice 10 fold cross validation.
## Models will be trained within the training 9 folds in a 10 -fold cv manner, and then validated in the left out
## 1/10th fold - 10 times.

## Now create a loop to go though each image modality, gender, and return the CVR^2
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'all.data')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_', '_jlf_')
allRVals <- NA
for(g in 1:2){
  for(z in 1:length(dataNames)){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    print(dim(tmp.values))
    # Now produce the beta weights
    outVals <- runRidgeOnAll(x=tmp.values, y=tmp.out)
    # Now get all of the R^2 values
    eNet <- cor(outVals[[1]], tmp.out)^2
    linReg <- cor(outVals[[2]], tmp.out)^2
    out <- cbind(g,dataNames[z], eNet, linReg)
    allRVals <- rbind(allRVals, out)
  }
}
write.csv(allRVals, 'enetCompareLinReg.csv', quote=F, row.names=F)

# Now run the previous loop 100 times and see what kind of variance we get in the r^2 vals
allRVals <- NA
for(q in seq(1,50)){
  for(g in 1:2){
    for(z in 1:length(dataNames)){
      tmpDF <- get(dataNames[z])
      tmpDF <- tmpDF[which(tmpDF$sex==g),]
      tmp.col <- grep(dataGrepNames[z], names(tmpDF))
      tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
      tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
      tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
      tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
      print(dim(tmp.values))
      # Now produce the beta weights
      outVals <- runRidgeOnAll(x=tmp.values, y=tmp.out)
      # Now get all of the R^2 values
      eNet <- cor(outVals[[1]], tmp.out)^2
      linReg <- cor(outVals[[2]], tmp.out)^2
      out <- cbind(g,dataNames[z], eNet, linReg)
      allRVals <- rbind(allRVals, out)
    }
  }
}

# Now plot a violin plot of the RVals
allRVals <- allRVals[-1,]
allRValsPlot <- as.data.frame(allRVals)
allRValsPlot <- melt(allRValsPlot, id.vars=c('g', 'V2'))
allRValsPlot$value <- as.numeric(as.character(allRValsPlot$value))
allPlot <- ggplot(allRValsPlot, aes(x=V2, y=value, color=g, fill=variable)) + 
    geom_violin() + 
    #stat_summary(fun.y=mean, geom="point", shape=23, position=position_dodge(.1)) + 
    #stat_summary(fun.y=median, geom="point", size=2, color="red", position=position_dodge(.1)) + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="R-Squared", x='Modality')

