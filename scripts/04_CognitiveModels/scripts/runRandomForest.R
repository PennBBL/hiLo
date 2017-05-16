# AFGR May 16 2017
# This script will be used to produce the importance level for each of the ROIs
# for the hi lo project. This will be done on the non modality regressed, age, and quality regressed data.


# Load library(s)
# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest')

# Load the data
# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')

# Now produce the importance value for our volume data
male.vol.data <- vol.data[which(vol.data$sex==1),]
vol.col <- grep('mprage_jlf_vol', names(vol.data))
male.vol.values <- scale(male.vol.data[,vol.col])[,1:length(vol.col)]
male.vol.outcome <- scale(male.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.vol.outcome <- male.vol.outcome[complete.cases(male.vol.data[,vol.col])]
male.vol.values <- male.vol.values[complete.cases(male.vol.data[,vol.col]),]
index <- unlist(createFolds(male.vol.outcome, k=3, list=T, returnTrain=T)[1])
maleVol <- randomForest(x=male.vol.values[index,], y=male.vol.outcome[index], xtest=male.vol.values[-index,],ytest=male.vol.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.vol.data <- vol.data[which(vol.data$sex==1),]
female.vol.values <- scale(female.vol.data[,vol.col])[,1:length(vol.col)]
female.vol.outcome <- scale(female.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.vol.outcome <- female.vol.outcome[complete.cases(female.vol.data[,vol.col])]
female.vol.values <- female.vol.values[complete.cases(female.vol.data[,vol.col]),]
index <- unlist(createFolds(female.vol.outcome, k=3, list=T, returnTrain=T)[1])
femaleVol <- randomForest(x=female.vol.values[index,], y=female.vol.outcome[index], xtest=female.vol.values[-index,],ytest=female.vol.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.cbf.data <- cbf.data[which(cbf.data$sex==1),]
cbf.col <- grep('jlf_cbf', names(cbf.data))
male.cbf.values <- scale(male.cbf.data[,cbf.col])[,1:length(cbf.col)]
male.cbf.outcome <- scale(male.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.cbf.outcome <- male.cbf.outcome[complete.cases(male.cbf.data[,cbf.col])]
male.cbf.values <- male.cbf.values[complete.cases(male.cbf.data[,cbf.col]),]
index <- unlist(createFolds(male.cbf.outcome, k=3, list=T, returnTrain=T)[1])
maleCbf <- randomForest(x=male.cbf.values[index,], y=male.cbf.outcome[index], xtest=male.cbf.values[-index,],ytest=male.cbf.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.cbf.data <- cbf.data[which(cbf.data$sex==2),]
cbf.col <- grep('jlf_cbf', names(cbf.data))
female.cbf.values <- scale(female.cbf.data[,cbf.col])[,1:length(cbf.col)]
female.cbf.outcome <- scale(female.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.cbf.outcome <- female.cbf.outcome[complete.cases(female.cbf.data[,cbf.col])]
female.cbf.values <- female.cbf.values[complete.cases(female.cbf.data[,cbf.col]),]
index <- unlist(createFolds(female.cbf.outcome, k=3, list=T, returnTrain=T)[1])
femaleCbf <- randomForest(x=female.cbf.values[index,], y=female.cbf.outcome[index], xtest=female.cbf.values[-index,],ytest=female.cbf.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.gmd.data <- gmd.data[which(gmd.data$sex==1),]
gmd.col <- grep('jlf_gmd', names(gmd.data))
male.gmd.values <- scale(male.gmd.data[,gmd.col])[,1:length(gmd.col)]
male.gmd.outcome <- scale(male.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.gmd.outcome <- male.gmd.outcome[complete.cases(male.gmd.data[,gmd.col])]
male.gmd.values <- male.gmd.values[complete.cases(male.gmd.data[,gmd.col]),]
index <- unlist(createFolds(male.gmd.outcome, k=3, list=T, returnTrain=T)[1])
maleGmd <- randomForest(x=male.gmd.values[index,], y=male.gmd.outcome[index], xtest=male.gmd.values[-index,],ytest=male.gmd.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.gmd.data <- gmd.data[which(gmd.data$sex==2),]
gmd.col <- grep('jlf_gmd', names(gmd.data))
female.gmd.values <- scale(female.gmd.data[,gmd.col])[,1:length(gmd.col)]
female.gmd.outcome <- scale(female.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.gmd.outcome <- female.gmd.outcome[complete.cases(female.gmd.data[,gmd.col])]
female.gmd.values <- female.gmd.values[complete.cases(female.gmd.data[,gmd.col]),]
index <- unlist(createFolds(female.gmd.outcome, k=3, list=T, returnTrain=T)[1])
femaleGmd <- randomForest(x=female.gmd.values[index,], y=female.gmd.outcome[index], xtest=female.gmd.values[-index,],ytest=female.gmd.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.ct.data <- ct.data[which(ct.data$sex==1),]
ct.col <- grep('jlf_ct', names(ct.data))
male.ct.values <- scale(male.ct.data[,ct.col])[,1:length(ct.col)]
male.ct.outcome <- scale(male.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ct.outcome <- male.ct.outcome[complete.cases(male.ct.data[,ct.col])]
male.ct.values <- male.ct.values[complete.cases(male.ct.data[,ct.col]),]
index <- unlist(createFolds(male.ct.outcome, k=3, list=T, returnTrain=T)[1])
maleCt <- randomForest(x=male.ct.values[index,], y=male.ct.outcome[index], xtest=male.ct.values[-index,],ytest=male.ct.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.ct.data <- ct.data[which(ct.data$sex==2),]
ct.col <- grep('jlf_ct', names(ct.data))
female.ct.values <- scale(female.ct.data[,ct.col])[,1:length(ct.col)]
female.ct.outcome <- scale(female.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.ct.outcome <- female.ct.outcome[complete.cases(female.ct.data[,ct.col])]
female.ct.values <- female.ct.values[complete.cases(female.ct.data[,ct.col]),]
index <- unlist(createFolds(female.ct.outcome, k=3, list=T, returnTrain=T)[1])
femaleCt <- randomForest(x=female.ct.values[index,], y=female.ct.outcome[index], xtest=female.ct.values[-index,],ytest=female.ct.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.reho.data <- reho.data[which(reho.data$sex==1),]
reho.col <- grep('jlf_reho', names(reho.data))
male.reho.values <- scale(male.reho.data[,reho.col])[,1:length(reho.col)]
male.reho.outcome <- scale(male.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.reho.outcome <- male.reho.outcome[complete.cases(male.reho.data[,reho.col])]
male.reho.values <- male.reho.values[complete.cases(male.reho.data[,reho.col]),]
index <- unlist(createFolds(male.reho.outcome, k=3, list=T, returnTrain=T)[1])
maleReho <- randomForest(x=male.reho.values[index,], y=male.reho.outcome[index], xtest=male.reho.values[-index,],ytest=male.reho.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.reho.data <- reho.data[which(reho.data$sex==2),]
reho.col <- grep('jlf_reho', names(reho.data))
female.reho.values <- scale(female.reho.data[,reho.col])[,1:length(reho.col)]
female.reho.outcome <- scale(female.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.reho.outcome <- female.reho.outcome[complete.cases(female.reho.data[,reho.col])]
female.reho.values <- female.reho.values[complete.cases(female.reho.data[,reho.col]),]
index <- unlist(createFolds(female.reho.outcome, k=3, list=T, returnTrain=T)[1])
femaleReho <- randomForest(x=female.reho.values[index,], y=female.reho.outcome[index], xtest=female.reho.values[-index,],ytest=female.reho.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.alff.data <- alff.data[which(alff.data$sex==1),]
alff.col <- grep('jlf_alff', names(alff.data))
male.alff.values <- scale(male.alff.data[,alff.col])[,1:length(alff.col)]
male.alff.outcome <- scale(male.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.alff.outcome <- male.alff.outcome[complete.cases(male.alff.data[,alff.col])]
male.alff.values <- male.alff.values[complete.cases(male.alff.data[,alff.col]),]
index <- unlist(createFolds(male.alff.outcome, k=3, list=T, returnTrain=T)[1])
maleAlff <- randomForest(x=male.alff.values[index,], y=male.alff.outcome[index], xtest=male.alff.values[-index,],ytest=male.alff.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.alff.data <- alff.data[which(alff.data$sex==2),]
alff.col <- grep('jlf_alff', names(alff.data))
female.alff.values <- scale(female.alff.data[,alff.col])[,1:length(alff.col)]
female.alff.outcome <- scale(female.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.alff.outcome <- female.alff.outcome[complete.cases(female.alff.data[,alff.col])]
female.alff.values <- female.alff.values[complete.cases(female.alff.data[,alff.col]),]
index <- unlist(createFolds(female.alff.outcome, k=3, list=T, returnTrain=T)[1])
femaleAlff <- randomForest(x=female.alff.values[index,], y=female.alff.outcome[index], xtest=female.alff.values[-index,],ytest=female.alff.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.tr.data <- tr.data[which(tr.data$sex==1),]
tr.col <- grep('jlf_tr', names(tr.data))
male.tr.values <- scale(male.tr.data[,tr.col])[,1:length(tr.col)]
male.tr.outcome <- scale(male.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.tr.outcome <- male.tr.outcome[complete.cases(male.tr.data[,tr.col])]
male.tr.values <- male.tr.values[complete.cases(male.tr.data[,tr.col]),]
index <- unlist(createFolds(male.tr.outcome, k=3, list=T, returnTrain=T)[1])
maleTr <- randomForest(x=male.tr.values[index,], y=male.tr.outcome[index], xtest=male.tr.values[-index,],ytest=male.tr.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.tr.data <- tr.data[which(tr.data$sex==2),]
tr.col <- grep('jlf_tr', names(tr.data))
female.tr.values <- scale(female.tr.data[,tr.col])[,1:length(tr.col)]
female.tr.outcome <- scale(female.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.tr.outcome <- female.tr.outcome[complete.cases(female.tr.data[,tr.col])]
female.tr.values <- female.tr.values[complete.cases(female.tr.data[,tr.col]),]
index <- unlist(createFolds(female.tr.outcome, k=3, list=T, returnTrain=T)[1])
femaleTr <- randomForest(x=female.tr.values[index,], y=female.tr.outcome[index], xtest=female.tr.values[-index,],ytest=female.tr.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.ad.data <- ad.data[which(ad.data$sex==1),]
ad.col <- grep('jlf_ad', names(ad.data))
male.ad.values <- scale(male.ad.data[,ad.col])[,1:length(ad.col)]
male.ad.outcome <- scale(male.ad.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ad.outcome <- male.ad.outcome[complete.cases(male.ad.data[,ad.col])]
male.ad.values <- male.ad.values[complete.cases(male.ad.data[,ad.col]),]
index <- unlist(createFolds(male.ad.outcome, k=3, list=T, returnTrain=T)[1])
maleAd <- randomForest(x=male.ad.values[index,], y=male.ad.outcome[index], xtest=male.ad.values[-index,],ytest=male.ad.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.ad.data <- ad.data[which(ad.data$sex==2),]
ad.col <- grep('jlf_ad', names(ad.data))
female.ad.values <- scale(female.ad.data[,ad.col])[,1:length(ad.col)]
female.ad.outcome <- scale(female.ad.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.ad.outcome <- female.ad.outcome[complete.cases(female.ad.data[,ad.col])]
female.ad.values <- female.ad.values[complete.cases(female.ad.data[,ad.col]),]
index <- unlist(createFolds(female.ad.outcome, k=3, list=T, returnTrain=T)[1])
femaleAd <- randomForest(x=female.ad.values[index,], y=female.ad.outcome[index], xtest=female.ad.values[-index,],ytest=female.ad.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

male.rd.data <- rd.data[which(rd.data$sex==1),]
rd.col <- grep('jlf_rd', names(rd.data))
male.rd.values <- scale(male.rd.data[,rd.col])[,1:length(rd.col)]
male.rd.outcome <- scale(male.rd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.rd.outcome <- male.rd.outcome[complete.cases(male.rd.data[,rd.col])]
male.rd.values <- male.rd.values[complete.cases(male.rd.data[,rd.col]),]
index <- unlist(createFolds(male.rd.outcome, k=3, list=T, returnTrain=T)[1])
maleRd <- randomForest(x=male.rd.values[index,], y=male.rd.outcome[index], xtest=male.rd.values[-index,],ytest=male.rd.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.rd.data <- rd.data[which(rd.data$sex==2),]
rd.col <- grep('jlf_rd', names(rd.data))
female.rd.values <- scale(female.rd.data[,rd.col])[,1:length(rd.col)]
female.rd.outcome <- scale(female.rd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.rd.outcome <- female.rd.outcome[complete.cases(female.rd.data[,rd.col])]
female.rd.values <- female.rd.values[complete.cases(female.rd.data[,rd.col]),]
index <- unlist(createFolds(female.rd.outcome, k=3, list=T, returnTrain=T)[1])
femaleRd <- randomForest(x=female.rd.values[index,], y=female.rd.outcome[index], xtest=female.rd.values[-index,],ytest=female.rd.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

output <- c(maleVol, maleCbf, maleGmd, maleCt, maleReho, maleAlff, maleTr, maleRd, maleAd)
tmp <- c(rep('Vol', length(maleVol)), rep('CBF', length(maleCbf)), rep('GMD', length(maleGmd)), rep('CT', length(maleCt)), rep('REHO', length(maleReho)), rep('ALFF', length(maleAlff)), rep('TR', length(maleTr)), rep('RD', length(maleRd)), rep('AD', length(maleAd)))
output <- cbind(output, tmp)
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'maleRFImportance.csv', quote=F, row.names=F)

output <- c(femaleVol, femaleCbf, femaleGmd, femaleCt, femaleReho, femaleAlff, femaleTr, femaleRd, femaleAd)
tmp <- c(rep('Vol', length(femaleVol)), rep('CBF', length(femaleCbf)), rep('GMD', length(femaleGmd)), rep('CT', length(femaleCt)), rep('REHO', length(femaleReho)), rep('ALFF', length(femaleAlff)), rep('TR', length(femaleTr)), rep('RD', length(femaleRd)), rep('AD', length(femaleAd)))
output <- cbind(output, tmp)
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'femaleRFImportance.csv', quote=F, row.names=F)

tmp <- merge(vol.data, cbf.data, by=c('bblid', 'scanid'))
tmp <- merge(tmp, gmd.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, ct.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, reho.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, alff.data,  by=c('bblid', 'scanid'))
#tmp <- merge(tmp, ad.data,  by=c('bblid', 'scanid'))
#tmp <- merge(tmp, rd.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, tr.data,  by=c('bblid', 'scanid'))
all.data <- tmp[,c(1,2, 8, 44,grep('jlf', names(tmp)))]

# Now perform the analysis
male.all.data <- all.data[which(all.data$sex.x==1),]
all.col <- grep('jlf', names(all.data))
male.all.values <- scale(male.all.data[,all.col])[,1:length(all.col)]
male.all.outcome <- scale(male.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.all.outcome <- male.all.outcome[complete.cases(male.all.data[,all.col])]
male.all.values <- male.all.values[complete.cases(male.all.data[,all.col]),]
index <- unlist(createFolds(male.all.outcome, k=3, list=T, returnTrain=T)[1])
maleAll <- randomForest(x=male.all.values[index,], y=male.all.outcome[index], xtest=male.all.values[-index,],ytest=male.all.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

