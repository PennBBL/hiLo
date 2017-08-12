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
vol.data <- vol.data[,-grep("4th_Ventricle", names(vol.data))]
#vol.data <- vol.data[which(vol.data$ageAtGo1Scan>=168 & vol.data$ageAtGo1Scan<216),]
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
#cbf.data <- cbf.data[which(cbf.data$ageAtGo1Scan>=168 & cbf.data$ageAtGo1Scan<216),]
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
#gmd.data <- gmd.data[which(gmd.data$ageAtGo1Scan>=168 & gmd.data$ageAtGo1Scan<216),]
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
#ct.data <- ct.data[which(ct.data$ageAtGo1Scan>=168 & ct.data$ageAtGo1Scan<216),]
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
#reho.data <- reho.data[which(reho.data$ageAtGo1Scan>=168 & reho.data$ageAtGo1Scan<216),]
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
#alff.data <- alff.data[which(alff.data$ageAtGo1Scan>=168 & alff.data$ageAtGo1Scan<216),]
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
ad.data <- ad.data[which(ad.data$ageAtGo1Scan>=168),]
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
fa.data <- fa.data[which(fa.data$ageAtGo1Scan>=168),]
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
rd.data <- rd.data[which(rd.data$ageAtGo1Scan>=168),]
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
#tr.data <- tr.data[which(tr.data$ageAtGo1Scan>=168 & tr.data$ageAtGo1Scan<216),]
fa.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabelsData.csv')
tr.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuTRLabelsData.csv')

# Now produce the importance value for our volume data
pdf('varImpPlotsTop20.pdf', height=30, width=20)
male.vol.data <- vol.data[which(vol.data$sex==1),]
vol.col <- grep('mprage_jlf_vol', names(vol.data))
male.vol.values <- scale(male.vol.data[,vol.col])[,1:length(vol.col)]
male.vol.outcome <- scale(male.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.vol.outcome <- male.vol.outcome[complete.cases(male.vol.data[,vol.col])]
male.vol.values <- male.vol.values[complete.cases(male.vol.data[,vol.col]),]
index <- unlist(createFolds(male.vol.outcome, k=3, list=T, returnTrain=T)[1])
maleVol <- randomForest(x=male.vol.values[index,], y=male.vol.outcome[index], xtest=male.vol.values[-index,],ytest=male.vol.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=3,proximity=T)
varImpPlot(maleVol)
maleVol1 <- importance(maleVol)[,1]
maleVol2 <- importance(maleVol)[,2]

female.vol.data <- vol.data[which(vol.data$sex==1),]
female.vol.values <- scale(female.vol.data[,vol.col])[,1:length(vol.col)]
female.vol.outcome <- scale(female.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.vol.outcome <- female.vol.outcome[complete.cases(female.vol.data[,vol.col])]
female.vol.values <- female.vol.values[complete.cases(female.vol.data[,vol.col]),]
index <- unlist(createFolds(female.vol.outcome, k=3, list=T, returnTrain=T)[1])
femaleVol <- randomForest(x=female.vol.values[index,], y=female.vol.outcome[index], xtest=female.vol.values[-index,],ytest=female.vol.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleVol)
femaleVol1 <- importance(femaleVol)[,1]
femaleVol2 <- importance(femaleVol)[,2]

male.cbf.data <- cbf.data[which(cbf.data$sex==1),]
cbf.col <- grep('jlf_cbf', names(cbf.data))
male.cbf.values <- scale(male.cbf.data[,cbf.col])[,1:length(cbf.col)]
male.cbf.outcome <- scale(male.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.cbf.outcome <- male.cbf.outcome[complete.cases(male.cbf.data[,cbf.col])]
male.cbf.values <- male.cbf.values[complete.cases(male.cbf.data[,cbf.col]),]
index <- unlist(createFolds(male.cbf.outcome, k=3, list=T, returnTrain=T)[1])
maleCbf <- randomForest(x=male.cbf.values[index,], y=male.cbf.outcome[index], xtest=male.cbf.values[-index,],ytest=male.cbf.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(maleCbf)
maleCbf1 <- importance(maleCbf)[,1]
maleCbf2 <- importance(maleCbf)[,2]

female.cbf.data <- cbf.data[which(cbf.data$sex==2),]
cbf.col <- grep('jlf_cbf', names(cbf.data))
female.cbf.values <- scale(female.cbf.data[,cbf.col])[,1:length(cbf.col)]
female.cbf.outcome <- scale(female.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.cbf.outcome <- female.cbf.outcome[complete.cases(female.cbf.data[,cbf.col])]
female.cbf.values <- female.cbf.values[complete.cases(female.cbf.data[,cbf.col]),]
index <- unlist(createFolds(female.cbf.outcome, k=3, list=T, returnTrain=T)[1])
femaleCbf <- randomForest(x=female.cbf.values[index,], y=female.cbf.outcome[index], xtest=female.cbf.values[-index,],ytest=female.cbf.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleCbf)
femaleCbf1 <- importance(femaleCbf)[,1]
femaleCbf2 <- importance(femaleCbf)[,2]

male.gmd.data <- gmd.data[which(gmd.data$sex==1),]
gmd.col <- grep('jlf_gmd', names(gmd.data))
male.gmd.values <- scale(male.gmd.data[,gmd.col])[,1:length(gmd.col)]
male.gmd.outcome <- scale(male.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.gmd.outcome <- male.gmd.outcome[complete.cases(male.gmd.data[,gmd.col])]
male.gmd.values <- male.gmd.values[complete.cases(male.gmd.data[,gmd.col]),]
index <- unlist(createFolds(male.gmd.outcome, k=3, list=T, returnTrain=T)[1])
maleGmd <- randomForest(x=male.gmd.values[index,], y=male.gmd.outcome[index], xtest=male.gmd.values[-index,],ytest=male.gmd.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(maleGmd)
maleGmd1 <- importance(maleGmd)[,1]
maleGmd2 <- importance(maleGmd)[,2]

female.gmd.data <- gmd.data[which(gmd.data$sex==2),]
gmd.col <- grep('jlf_gmd', names(gmd.data))
female.gmd.values <- scale(female.gmd.data[,gmd.col])[,1:length(gmd.col)]
female.gmd.outcome <- scale(female.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.gmd.outcome <- female.gmd.outcome[complete.cases(female.gmd.data[,gmd.col])]
female.gmd.values <- female.gmd.values[complete.cases(female.gmd.data[,gmd.col]),]
index <- unlist(createFolds(female.gmd.outcome, k=3, list=T, returnTrain=T)[1])
femaleGmd <- randomForest(x=female.gmd.values[index,], y=female.gmd.outcome[index], xtest=female.gmd.values[-index,],ytest=female.gmd.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleGmd)
femaleGmd1 <- importance(femaleGmd)[,1]
femaleGmd2 <- importance(femaleGmd)[,2]

male.ct.data <- ct.data[which(ct.data$sex==1),]
ct.col <- grep('jlf_ct', names(ct.data))
male.ct.values <- scale(male.ct.data[,ct.col])[,1:length(ct.col)]
male.ct.outcome <- scale(male.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ct.outcome <- male.ct.outcome[complete.cases(male.ct.data[,ct.col])]
male.ct.values <- male.ct.values[complete.cases(male.ct.data[,ct.col]),]
index <- unlist(createFolds(male.ct.outcome, k=3, list=T, returnTrain=T)[1])
maleCt <- randomForest(x=male.ct.values[index,], y=male.ct.outcome[index], xtest=male.ct.values[-index,],ytest=male.ct.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(maleCt)
maleCt1 <- importance(maleCt)[,1]
maleCt2 <- importance(maleCt)[,2]

female.ct.data <- ct.data[which(ct.data$sex==2),]
ct.col <- grep('jlf_ct', names(ct.data))
female.ct.values <- scale(female.ct.data[,ct.col])[,1:length(ct.col)]
female.ct.outcome <- scale(female.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.ct.outcome <- female.ct.outcome[complete.cases(female.ct.data[,ct.col])]
female.ct.values <- female.ct.values[complete.cases(female.ct.data[,ct.col]),]
index <- unlist(createFolds(female.ct.outcome, k=3, list=T, returnTrain=T)[1])
femaleCt <- randomForest(x=female.ct.values[index,], y=female.ct.outcome[index], xtest=female.ct.values[-index,],ytest=female.ct.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleCt)
femaleCt1 <- importance(femaleCt)[,1]
femaleCt2 <- importance(femaleCt)[,2]

male.reho.data <- reho.data[which(reho.data$sex==1),]
reho.col <- grep('jlf_reho', names(reho.data))
male.reho.values <- scale(male.reho.data[,reho.col])[,1:length(reho.col)]
male.reho.outcome <- scale(male.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.reho.outcome <- male.reho.outcome[complete.cases(male.reho.data[,reho.col])]
male.reho.values <- male.reho.values[complete.cases(male.reho.data[,reho.col]),]
index <- unlist(createFolds(male.reho.outcome, k=3, list=T, returnTrain=T)[1])
maleReho <- randomForest(x=male.reho.values[index,], y=male.reho.outcome[index], xtest=male.reho.values[-index,],ytest=male.reho.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(maleReho)
maleReho1 <- importance(maleReho)[,1]
maleReho2 <- importance(maleReho)[,2]

female.reho.data <- reho.data[which(reho.data$sex==2),]
reho.col <- grep('jlf_reho', names(reho.data))
female.reho.values <- scale(female.reho.data[,reho.col])[,1:length(reho.col)]
female.reho.outcome <- scale(female.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.reho.outcome <- female.reho.outcome[complete.cases(female.reho.data[,reho.col])]
female.reho.values <- female.reho.values[complete.cases(female.reho.data[,reho.col]),]
index <- unlist(createFolds(female.reho.outcome, k=3, list=T, returnTrain=T)[1])
femaleReho <- randomForest(x=female.reho.values[index,], y=female.reho.outcome[index], xtest=female.reho.values[-index,],ytest=female.reho.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleReho)
femaleReho1 <- importance(femaleReho)[,1]
femaleReho2 <- importance(femaleReho)[,2]

male.alff.data <- alff.data[which(alff.data$sex==1),]
alff.col <- grep('jlf_alff', names(alff.data))
male.alff.values <- scale(male.alff.data[,alff.col])[,1:length(alff.col)]
male.alff.outcome <- scale(male.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.alff.outcome <- male.alff.outcome[complete.cases(male.alff.data[,alff.col])]
male.alff.values <- male.alff.values[complete.cases(male.alff.data[,alff.col]),]
index <- unlist(createFolds(male.alff.outcome, k=3, list=T, returnTrain=T)[1])
maleAlff <- randomForest(x=male.alff.values[index,], y=male.alff.outcome[index], xtest=male.alff.values[-index,],ytest=male.alff.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(maleAlff)
maleAlff1 <- importance(maleAlff)[,1]
maleAlff2 <- importance(maleAlff)[,2]

female.alff.data <- alff.data[which(alff.data$sex==2),]
alff.col <- grep('jlf_alff', names(alff.data))
female.alff.values <- scale(female.alff.data[,alff.col])[,1:length(alff.col)]
female.alff.outcome <- scale(female.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.alff.outcome <- female.alff.outcome[complete.cases(female.alff.data[,alff.col])]
female.alff.values <- female.alff.values[complete.cases(female.alff.data[,alff.col]),]
index <- unlist(createFolds(female.alff.outcome, k=3, list=T, returnTrain=T)[1])
femaleAlff <- randomForest(x=female.alff.values[index,], y=female.alff.outcome[index], xtest=female.alff.values[-index,],ytest=female.alff.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleAlff)
femaleAlff1 <- importance(femaleAlff)[,1]
femaleAlff2 <- importance(femaleAlff)[,2]

male.tr.data <- tr.data[which(tr.data$sex==1),]
tr.col <- grep('jlf_tr', names(tr.data))
male.tr.values <- scale(male.tr.data[,tr.col])[,1:length(tr.col)]
male.tr.outcome <- scale(male.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.tr.outcome <- male.tr.outcome[complete.cases(male.tr.data[,tr.col])]
male.tr.values <- male.tr.values[complete.cases(male.tr.data[,tr.col]),]
index <- unlist(createFolds(male.tr.outcome, k=3, list=T, returnTrain=T)[1])
maleTr <- randomForest(x=male.tr.values[index,], y=male.tr.outcome[index], xtest=male.tr.values[-index,],ytest=male.tr.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(maleTr)
maleTr1 <- importance(maleTr)[,1]
maleTr2 <- importance(maleTr)[,2]

female.tr.data <- tr.data[which(tr.data$sex==2),]
tr.col <- grep('jlf_tr', names(tr.data))
female.tr.values <- scale(female.tr.data[,tr.col])[,1:length(tr.col)]
female.tr.outcome <- scale(female.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.tr.outcome <- female.tr.outcome[complete.cases(female.tr.data[,tr.col])]
female.tr.values <- female.tr.values[complete.cases(female.tr.data[,tr.col]),]
index <- unlist(createFolds(female.tr.outcome, k=3, list=T, returnTrain=T)[1])
femaleTr <- randomForest(x=female.tr.values[index,], y=female.tr.outcome[index], xtest=female.tr.values[-index,],ytest=female.tr.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)
varImpPlot(femaleTr)
femaleTr1 <- importance(femaleTr)[,1]
femaleTr2 <- importance(femaleTr)[,2]
dev.off()

output <- c(maleVol1, maleCbf1, maleGmd1, maleCt1, maleReho1, maleAlff1, maleTr1)
tmp <- c(rep('Vol', length(maleVol1)), rep('CBF', length(maleCbf1)), rep('GMD', length(maleGmd1)), rep('CT', length(maleCt1)), rep('REHO', length(maleReho1)), rep('ALFF', length(maleAlff1)), rep('TR', length(maleTr1)))
output <- cbind(output, tmp)
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'maleRFImportance.csv', quote=F, row.names=F)

output <- c(maleVol2, maleCbf2, maleGmd2, maleCt2, maleReho2, maleAlff2, maleTr2)
tmp <- c(rep('Vol', length(maleVol2)), rep('CBF', length(maleCbf2)), rep('GMD', length(maleGmd2)), rep('CT', length(maleCt2)), rep('REHO', length(maleReho2)), rep('ALFF', length(maleAlff2)), rep('TR', length(maleTr2)))
output <- cbind(output, tmp)
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'maleRFImportance2.csv', quote=F, row.names=F)

output <- c(femaleVol1, femaleCbf1, femaleGmd1, femaleCt1, femaleReho1, femaleAlff1, femaleTr1)
tmp <- c(rep('Vol', length(femaleVol1)), rep('CBF', length(femaleCbf1)), rep('GMD', length(femaleGmd1)), rep('CT', length(femaleCt1)), rep('REHO', length(femaleReho1)), rep('ALFF', length(femaleAlff1)), rep('TR', length(femaleTr1)))
output <- cbind(output, tmp)
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'femaleRFImportance.csv', quote=F, row.names=F)

output <- c(femaleVol2, femaleCbf2, femaleGmd2, femaleCt2, femaleReho2, femaleAlff2, femaleTr2)
tmp <- c(rep('Vol', length(femaleVol1)), rep('CBF', length(femaleCbf1)), rep('GMD', length(femaleGmd1)), rep('CT', length(femaleCt1)), rep('REHO', length(femaleReho1)), rep('ALFF', length(femaleAlff1)), rep('TR', length(femaleTr1)))
output <- cbind(output, tmp)
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'femaleRFImportance2.csv', quote=F, row.names=F)

# Now run forward step wise regression for the all data model
tmp <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
tmp <- merge(tmp, gmd.data,  by=intersect(names(tmp), names(gmd.data)))
tmp <- merge(tmp, ct.data,  by=intersect(names(tmp), names(ct.data)))
#tmp <- merge(tmp, reho.data,  by=intersect(names(tmp), names(reho.data)))
#tmp <- merge(tmp, alff.data,  by=intersect(names(tmp), names(alff.data)))
tmp <- merge(tmp, tr.data,  by=intersect(names(tmp), names(tr.data)))
tmp <- merge(tmp, fa.data.label, by=intersect(names(tmp), names(fa.data.label)))
tmp <- merge(tmp, tr.data.label, by=intersect(names(tmp), names(tr.data.label)))
all.data <- tmp
colnames(all.data) <- gsub(x=colnames(all.data), pattern='dti_dtitk_jhulabel', replacement='jlf_dti_dtitk_jhulabel_fa')

# Now perform the analysis
male.all.data <- all.data[which(all.data$sex==1),]
all.col <- grep('jlf', names(all.data))
male.all.values <- scale(male.all.data[,all.col])[,1:length(all.col)]
male.all.outcome <- scale(male.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.all.outcome <- male.all.outcome[complete.cases(male.all.data[,all.col])]
male.all.values <- male.all.values[complete.cases(male.all.data[,all.col]),]
index <- unlist(createFolds(male.all.outcome, k=3, list=T, returnTrain=T)[1])
maleAll <- randomForest(x=male.all.values[index,], y=male.all.outcome[index], xtest=male.all.values[-index,],ytest=male.all.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

female.all.data <- all.data[which(all.data$sex==2),]
female.all.values <- scale(female.all.data[,all.col])[,1:length(all.col)]
female.all.outcome <- scale(female.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.all.outcome <- female.all.outcome[complete.cases(female.all.data[,all.col])]
female.all.values <- female.all.values[complete.cases(female.all.data[,all.col]),]
index <- unlist(createFolds(female.all.outcome, k=3, list=T, returnTrain=T)[1])
femaleAll <- randomForest(x=female.all.values[index,], y=female.all.outcome[index], xtest=female.all.values[-index,],ytest=female.all.outcome[-index], importance=T,keep.forest=T, ntree=1000,nPerm=2,proximity=T)$importance[,2]

# Now write the csv
allOutput <- cbind(maleAll, femaleAll)
write.csv(allOutput, 'allVariableImportance.csv', quote=F, row.names=T)
