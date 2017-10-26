source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')

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

all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))

# Now for males run the tpot method 100 times for each modality and build a violin l=plot of the cv r2 values
#cl1 <- makeCluster(8)
#registerDoParallel(cl1)
allVals <- foreach(i=c('vol.data','cbf.data','gmd.data','reho.data','tr.data','all.data'), .combine='rbind',.errorhandling = 'remove',.export=ls(envir=globalenv())) %dopar% {
    # Source all needed functions et cetra 
    source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
    source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
    install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')

    tmpDat <- get(i)
    # Now produce the cv r squared
    tmpOut <- NULL
    for(z in seq(1, 3)){
      val <- runTpotModel(tmpDat, 1)[2]
      outRow <- c(i, z, val)
      tmpOut <- rbind(tmpOut, outRow)
    }
    tmpOut

}
stopCluster(cl1)

allValsMale <- as.data.frame(allVals)
allValsMale$V3 <- as.numeric(as.character(allValsMale$V3))
outPlot <- ggplot(allValsMale, aes(x=V1, y=V3)) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=23) + 
    stat_summary(fun.y=median, geom="point", size=2, color="red") + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="CV R Squared", x='Modality')

pdf('maleCVValues.pdf')
print(outPlot)
dev.off()




# Now do female
cl1 <- makeCluster(8)
registerDoParallel(cl1)
allVals <- foreach(i=c('vol.data','cbf.data','gmd.data','reho.data','tr.data','all.data'), .combine='rbind',.errorhandling = 'remove',.export=ls(envir=globalenv())) %dopar% {
    # Source all needed functions et cetra 
    source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
    source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
    install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')

    tmpDat <- get(i)
    # Now produce the cv r squared
    tmpOut <- NULL
    for(z in seq(1, 3)){
      val <- runTpotModel(tmpDat, 2)[2]
      outRow <- c(i, z, val)
      tmpOut <- rbind(tmpOut, outRow)
    }
    tmpOut

}
stopCluster(cl1)
stopCluster(cl1)
allValsMale <- as.data.frame(allVals)
allValsMale$V3 <- as.numeric(as.character(allValsMale$V3))
outPlot <- ggplot(allValsMale, aes(x=V1, y=V3)) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=23) + 
    stat_summary(fun.y=median, geom="point", size=2, color="red") + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="CV R Squared", x='Modality')

pdf('femaleCVValues.pdf')
print(outPlot)
dev.off()
