source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2')


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
all.data <- merge(all.data, cc.data)

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

# Now try austin on all
# First remove the summary variables
all.data.var.select <- all.data
valsToRM <- grep('_Mean', names(all.data.var.select))
valsToRM <- c(grep('_ICV', names(all.data.var.select)), valsToRM)
all.data.var.select <- all.data.var.select[,-valsToRM]
selectVals <- returnSelectionN(all.data.var.select, grepID='_jlf_', genderID=1, nCor=25, dir='backward', iterationCount=50)
tmp <- all.data[which(all.data$sex==1),grep('_jlf_', names(all.data))]
namesToUse <- rownames(selectVals)[which(returnSelectionCol(selectVals)>24)]
index <- colnames(tmp) %in% namesToUse
tmpP <- tmp[,index]
foo <- runRidgeOnAll(x=tmpP, y=all.data$F1_Exec_Comp_Cog_Accuracy[which(all.data$sex==1)])

# Now run the previous loop 100 times and see what kind of variance we get in the r^2 vals
# Also create a vector with the summary metrics so we now what to grep for and rm
allRVals <- NULL
vol.data.step <- vol.data[,-grep('mprage_jlf_vol_ICV', names(vol.data))]
cbf.data.step <- cbf.data[,-grep('pcasl_jlf_cbf_MeanGM', names(cbf.data))]
gmd.data.step <- gmd.data[,-grep('mprage_jlf_gmd_MeanGMD', names(gmd.data))]
tr.data.step <- tr.data[,-grep('dti_jlf_tr_MeanTR', names(tr.data))]
data.step <- c('vol.data.step', 'cbf.data.step', 'gmd.data.step', 'tr.data.step', 'all.data.var.select')
for(q in seq(1,10)){
  for(g in 1:2){
    for(z in 1:4){
      print(q)
      tmpDF <- get(dataNames[z])
      tmpDF <- tmpDF[which(tmpDF$sex==g),]
      tmp.col <- grep(dataGrepNames[z], names(tmpDF))
      tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
      tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
      tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
      tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
      if(z > 0){
        selectVals <- returnSelectionN(dataFrame=get(data.step[z]), grepID=dataGrepNames[z], genderID=g, nCor=10, dir='forward', iterationCount=51)
        namesToUse <- rownames(selectVals)[which(returnSelectionCol(selectVals)>(dim(selectVals)[2]/2))]
        index <- colnames(tmp.values) %in% namesToUse
        tmp.values <- tmp.values[,index]
      }
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
allRValsPlot <- as.data.frame(allRVals)
allRValsPlot <- melt(allRValsPlot, id.vars=c('g', 'V2'))
allRValsPlot$value <- as.numeric(as.character(allRValsPlot$value))
allPlot <- ggplot(allRValsPlot, aes(x=V2, y=value, color=g, fill=variable)) + 
    geom_violin() + 
    #stat_summary(fun.y=mean, geom="point", shape=23, position=position_dodge(.1)) + 
    #stat_summary(fun.y=median, geom="point", size=2, color="red", position=position_dodge(.1)) + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="R-Squared", x='Modality') + 
    coord_cartesian(ylim=c(0, .3), expand=T)
pdf('backwardVarSelectOutVals.pdf')
print(allPlot)
dev.off()

# Now I need to explore selection trends within each modality
data.step <- c('vol.data.step', 'cbf.data.step', 'gmd.data.step', 'tr.data.step', 'all.data.var.select')
output <- NULL
for(g in 1:2){
  for(z in 1:4){
    outVal <- NULL
    for(p in 1:10){
      selectVals <- returnSelectionN(dataFrame=get(data.step[z]), grepID=dataGrepNames[z], genderID=g, nCor=25, dir='backward', iterationCount=100)
      outputVals <- returnSelectionCol(selectVals)
      outVal <- cbind(outVal, outputVals) 
      print(g,z,p)          
    }
    rownames(outVal) <- rownames(selectVals)
    outMean <- rowMeans(outVal)
    outSD <- apply(outVal, 1, sd)
    toWrite <- cbind(outMean, outSD)
    outCSV <- paste(g,z,'selectVals.csv', sep='')
    write.csv(toWrite, outCSV, row.names=T, quote=F)
  }  
}

# Now try the step wise austin heirarchy model building 
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'tr.data', 'all.data')
data.step <- c('vol.data.step', 'cbf.data.step', 'gmd.data.step', 'tr.data.step', 'all.data.var.select')
dataGrepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_', '_jlf_')
allRVals <- NA
pdf('outCVTrends.pdf')
for(g in 1:2){
  for(z in 1:5){
    tmpDF <- get(dataNames[z])
    tmpDF <- tmpDF[which(tmpDF$sex==g),]
    tmp.col <- grep(dataGrepNames[z], names(tmpDF))
    tmp.values <- scale(tmpDF[,tmp.col])[,1:length(tmp.col)]
    tmp.out <- scale(tmpDF$F1_Exec_Comp_Cog_Accuracy)
    tmp.out <- tmp.out[complete.cases(tmpDF[,tmp.col])]
    tmp.values <- tmp.values[complete.cases(tmpDF[,tmp.col]),]
    # Produce the selection count 
    selectN <- returnSelectionN(dataFrame=get(data.step[z]), grepID=dataGrepNames[z], genderID = g, nCor=25, iterationCount=100)
    # Now build the model
    # Now get all of the R^2 values
    if(z < 5){
      modelOut <- buildAustinModel(selectN, predVals=tmp.values, outVals=tmp.out, breakValue=1.1, nIters=dim(selectN)[1], stepSize=5)
    }
    if(z==5){
      modelOut <- buildAustinModel(selectN, predVals=tmp.values, outVals=tmp.out, breakValue=1, nIters=dim(selectN)[1], stepSize=10)
    }
    print(plot(modelOut[[2]], main=data.step[z], ylab='CV R-Squared'))
    allRVals <- rbind(allRVals, modelOut[[1]])
  }
}
dev.off()

