# AFGR September 12 2016
# This script is going to be used to run the variable selection for the hi lo paper
# It will perform this for each modality individually and then all of them together
# It is important to also note that this is performed within sex

# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest')

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
fa.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabelsData.csv')
tr.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuTRLabelsData.csv')

## Lets do volume data frist
# start with male data
male.vol.data <- vol.data[which(vol.data$sex==1),]
vol.col <- grep('mprage_jlf_vol', names(vol.data))
male.vol.values <- scale(male.vol.data[,vol.col])[,1:length(vol.col)]
male.vol.outcome <- scale(male.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.vol.outcome <- male.vol.outcome[complete.cases(male.vol.data[,vol.col])]
male.vol.values <- male.vol.values[complete.cases(male.vol.data[,vol.col]),]
# Now run the variable selection
maleVolBetaMatrix <- runLassoforHiLo(male.vol.values, male.vol.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.95, .95, .05)){
  maleVolValues <- rmFat(maleVolBetaMatrix, male.vol.values, val)
  maleVolValues <- regressWithinModality(maleVolValues, grepPattern='mprage_jlf_vol')
  maleVolFitStats <- computeModelFitMetrics(returnBetas=F,x = maleVolValues, y= male.vol.outcome)
  tmpOut <- rbind(tmpOut, maleVolFitStats)
}
maleVolOut <- tmpOut
write.csv(maleVolOut, 'maleQuantCutOffVol.csv', quote=F, row.names=F)

# Now do female
female.vol.data <- vol.data[which(vol.data$sex==2),]
female.vol.values <- scale(female.vol.data[,vol.col])[,1:length(vol.col)]
female.vol.outcome <- scale(female.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleVolBetaMatrix <- runLassoforHiLo(female.vol.values, female.vol.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.95, .95, .05)){
  femaleVolValues <- rmFat(femaleVolBetaMatrix, female.vol.values, val)
  femaleVolValues <- regressWithinModality(femaleVolValues, grep='mprage_jlf_vol')
  femaleVolFitStats <- computeModelFitMetrics(returnBetas=F,x = femaleVolValues, y= female.vol.outcome)
  tmpOut <- rbind(tmpOut, femaleVolFitStats)
}
femaleVolOut <- tmpOut
write.csv(femaleVolOut, 'femaleQuantCutOffVol.csv', quote=F, row.names=F)

## Now do CBF
# start with male
male.cbf.data <- cbf.data[which(cbf.data$sex==1),]
cbf.col <- grep('pcasl_jlf_cbf', names(male.cbf.data))
male.cbf.values <- scale(male.cbf.data[,cbf.col])[,1:length(cbf.col)]
male.cbf.outcome <- scale(male.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.cbf.outcome <- male.cbf.outcome[complete.cases(male.cbf.data[,cbf.col])]
male.cbf.values <- male.cbf.values[complete.cases(male.cbf.data[,cbf.col]),]

# Now run the variable selection 
maleCbfBetaMatrix <- runLassoforHiLo(male.cbf.values, male.cbf.outcome, nCor=25,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.95, .95, .05)){
  maleCbfValues <- rmFat(maleCbfBetaMatrix, male.cbf.values, val)
  maleCbfValues <- regressWithinModality(maleCbfValues, grep='pcasl_jlf_cbf')
  maleCbfFitStats <- computeModelFitMetrics(returnBetas=F,x = maleCbfValues, y= male.cbf.outcome)
  tmpOut <- rbind(tmpOut, maleCbfFitStats)
}
maleCbfOut <- tmpOut
write.csv(maleCbfOut, 'maleQuantCutOffCBf.csv', quote=F, row.names=F)

# Now do female
female.cbf.data <- cbf.data[which(cbf.data$sex==2),]
female.cbf.values <- scale(female.cbf.data[,cbf.col])[,1:length(cbf.col)]
female.cbf.outcome <- scale(female.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.cbf.outcome <- female.cbf.outcome[complete.cases(female.cbf.data[,cbf.col])]
female.cbf.values <- female.cbf.values[complete.cases(female.cbf.data[,cbf.col]),]

# Now run the variable selection 
femaleCbfBetaMatrix <- runLassoforHiLo(female.cbf.values, female.cbf.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.95, .95, .05)){
  femaleCbfValues <- rmFat(femaleCbfBetaMatrix, female.cbf.values, val)
  femaleCbfValues <- regressWithinModality(femaleCbfValues, grep='pcasl_jlf_cbf')
  femaleCbfFitStats <- computeModelFitMetrics(returnBetas=F,x = femaleCbfValues, y= female.cbf.outcome)
  tmpOut <- rbind(tmpOut, femaleCbfFitStats)
}
femaleCbfOut <- tmpOut
write.csv(femaleCbfOut, 'femaleQuantCutOffCbf.csv', quote=F, row.names=F)

## Now do GMD
male.gmd.data <- gmd.data[which(gmd.data$sex==1),]
gmd.col <- grep('mprage_jlf_gmd', names(male.gmd.data))
male.gmd.values <- scale(male.gmd.data[,gmd.col])[,1:length(gmd.col)]
male.gmd.outcome <- scale(male.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.gmd.outcome <- male.gmd.outcome[complete.cases(male.gmd.data[,gmd.col])]
male.gmd.values <- male.gmd.values[complete.cases(male.gmd.data[,gmd.col]),]

# Now run the variable selection 
maleGmdBetaMatrix <- runLassoforHiLo(male.gmd.values, male.gmd.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.95, .95, .05)){
  maleGmdValues <- rmFat(maleGmdBetaMatrix, male.gmd.values, val)
  maleGmdValues <- regressWithinModality(maleGmdValues, 'mprage_jlf_gmd')
  maleGmdFitStats <- computeModelFitMetrics(returnBetas=F,x = maleGmdValues, y= male.gmd.outcome)
  tmpOut <- rbind(tmpOut, maleGmdFitStats)
}
maleGmdOut <- tmpOut
write.csv(maleGmdOut, 'maleQuantCutOffGmd.csv', quote=F, row.names=F)

# Now do female
female.gmd.data <- gmd.data[which(gmd.data$sex==2),]
female.gmd.values <- scale(female.gmd.data[,gmd.col])[,1:length(gmd.col)]
female.gmd.outcome <- scale(female.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.gmd.outcome <- female.gmd.outcome[complete.cases(female.gmd.data[,gmd.col])]
female.gmd.values <- female.gmd.values[complete.cases(female.gmd.data[,gmd.col]),]

femaleGmdBetaMatrix <- runLassoforHiLo(female.gmd.values, female.gmd.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.95, .95, .05)){
  femaleGmdValues <- rmFat(femaleGmdBetaMatrix, female.gmd.values, .95)
  femaleGmdValues <- regressWithinModality(femaleGmdValues, 'mprage_jlf_gmd')
  femaleGmdFitStats <- computeModelFitMetrics(returnBetas=F,x = femaleGmdValues, y= female.gmd.outcome)
  tmpOut <- rbind(tmpOut, femaleGmdFitStats)
}
femaleGmdOut <- tmpOut
write.csv(femaleGmdOut, 'femaleQuantCutOffGmd.csv', quote=F, row.names=F)

## Now on to CT
male.ct.data <- ct.data[which(ct.data$sex==1),]
ct.col <- grep('mprage_jlf_ct', names(male.ct.data))
male.ct.values <- scale(male.ct.data[,ct.col])[,1:length(ct.col)]
male.ct.outcome <- scale(male.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ct.outcome <- male.ct.outcome[complete.cases(male.ct.data[,ct.col])]
male.ct.values <- male.ct.values[complete.cases(male.ct.data[,ct.col]),]

# Now run the variable selection 
maleCtBetaMatrix <- runLassoforHiLo(male.ct.values, male.ct.outcome, nCor=30,alphaSequence=.5)
maleCtValues <- rmFat(maleCtBetaMatrix, male.ct.values, .95)
maleCtValues <- regressWithinModality(maleCtValues, 'mprage_jlf_ct')
maleCtFitStats <- computeModelFitMetrics(returnBetas=T,x = maleCtValues, y= male.ct.outcome)

# And now onto female
female.ct.data <- ct.data[which(ct.data$sex==2),]
female.ct.values <- scale(female.ct.data[,ct.col])[,1:length(ct.col)]
female.ct.outcome <- scale(female.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.ct.outcome <- female.ct.outcome[complete.cases(female.ct.data[,ct.col])]
female.ct.values <- female.ct.values[complete.cases(female.ct.data[,ct.col]),]

femaleCtBetaMatrix <- runLassoforHiLo(female.ct.values, female.ct.outcome, nCor=30,alphaSequence=.5)
femaleCtValues <- rmFat(femaleCtBetaMatrix, female.ct.values, .95)
femaleCtValues <- regressWithinModality(femaleCtValues, 'mprage_jlf_ct')
femaleCtFitStats <- computeModelFitMetrics(returnBetas=T,x= femaleCtValues, y = female.ct.outcome)

## Now run reho
# start with male data
male.reho.data <- reho.data[which(reho.data$sex==1),]
reho.col <- grep('rest_jlf_reho', names(reho.data))
male.reho.values <- scale(male.reho.data[,reho.col])[,1:length(reho.col)]
male.reho.outcome <- scale(male.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.reho.outcome <- male.reho.outcome[complete.cases(male.reho.data[,reho.col])]
male.reho.values <- male.reho.values[complete.cases(male.reho.data[,reho.col]),]
# Now run the variable selection
maleRehoBetaMatrix <- runLassoforHiLo(male.reho.values, male.reho.outcome, nCor=30,alphaSequence=.5)
maleRehoValues <- rmFat(maleRehoBetaMatrix, male.reho.values, .95)
maleRehoValues <- regressWithinModality(maleRehoValues, 'rest_jlf_reho')
maleRehoFitStats <- computeModelFitMetrics(returnBetas=T,x = maleRehoValues, y= male.reho.outcome)

# Now do female
female.reho.data <- reho.data[which(reho.data$sex==2),]
female.reho.values <- scale(female.reho.data[,reho.col])[,1:length(reho.col)]
female.reho.outcome <- scale(female.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.reho.outcome <- female.reho.outcome[complete.cases(female.reho.data[,reho.col])]
female.reho.values <- female.reho.values[complete.cases(female.reho.data[,reho.col]),]

# Now run the variable selection
femaleRehoBetaMatrix <- runLassoforHiLo(female.reho.values, female.reho.outcome, nCor=30,alphaSequence=.5)
femaleRehoValues <- rmFat(femaleRehoBetaMatrix, female.reho.values, .95)
femaleRehoValues <- regressWithinModality(femaleRehoValues, 'rest_jlf_reho')
femaleRehoFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleRehoValues, y= female.reho.outcome)

## Now onto alff
male.alff.data <- alff.data[which(alff.data$sex==1),]
alff.col <- grep('rest_jlf_alff', names(alff.data))
male.alff.values <- scale(male.alff.data[,alff.col])[,1:length(alff.col)]
male.alff.outcome <- scale(male.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.alff.outcome <- male.alff.outcome[complete.cases(male.alff.data[,alff.col])]
male.alff.values <- male.alff.values[complete.cases(male.alff.data[,alff.col]),]
# Now run the variable selection
maleAlffBetaMatrix <- runLassoforHiLo(male.alff.values, male.alff.outcome, nCor=30,alphaSequence=.5)
maleAlffValues <- rmFat(maleAlffBetaMatrix, male.alff.values, .95)
maleAlffValues <- regressWithinModality(maleAlffValues, 'rest_jlf_alff')
maleAlffFitStats <- computeModelFitMetrics(returnBetas=T,x = maleAlffValues, y= male.alff.outcome)

# Now do female
female.alff.data <- alff.data[which(alff.data$sex==2),]
female.alff.values <- scale(female.alff.data[,alff.col])[,1:length(alff.col)]
female.alff.outcome <- scale(female.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.alff.outcome <- female.alff.outcome[complete.cases(female.alff.data[,alff.col])]
female.alff.values <- female.alff.values[complete.cases(female.alff.data[,alff.col]),]

# Now run the variable selection
femaleAlffBetaMatrix <- runLassoforHiLo(female.alff.values, female.alff.outcome, nCor=30,alphaSequence=.5)
femaleAlffValues <- rmFat(femaleAlffBetaMatrix, female.alff.values, .95)
femaleAlffValues <- regressWithinModality(femaleAlffValues, 'rest_jlf_alff')
femaleAlffFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleAlffValues, y= female.alff.outcome)

# Now do TR
male.tr.data <- tr.data[which(tr.data$sex==1),]
tr.col <- grep('dti_jlf_tr', names(tr.data))
male.tr.values <- scale(male.tr.data[,tr.col])[,1:length(tr.col)]
male.tr.outcome <- scale(male.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.tr.outcome <- male.tr.outcome[complete.cases(male.tr.data[,tr.col])]
male.tr.values <- male.tr.values[complete.cases(male.tr.data[,tr.col]),]
# Now run the variable selection
maleTrBetaMatrix <- runLassoforHiLo(male.tr.values, male.tr.outcome, nCor=30,alphaSequence=.5)
maleTrValues <- rmFat(maleTrBetaMatrix, male.tr.values, .95)
maleTrValues <- regressWithinModality(maleTrValues, 'dti_jlf_tr')
maleTrFitStats <- computeModelFitMetrics(returnBetas=T,x = maleTrValues, y= male.tr.outcome)

# Now do female
female.tr.data <- tr.data[which(tr.data$sex==2),]
female.tr.values <- scale(female.tr.data[,tr.col])[,1:length(tr.col)]
female.tr.outcome <- scale(female.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleTrBetaMatrix <- runLassoforHiLo(female.tr.values, female.tr.outcome, nCor=30,alphaSequence=.5)
femaleTrValues <- rmFat(femaleTrBetaMatrix, female.tr.values, .95)
femaleTrValues <- regressWithinModality(femaleTrValues, 'dti_jlf_tr')
femaleTrFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleTrValues, y= female.tr.outcome)

# Now do AD
male.ad.data <- ad.data[which(ad.data$sex==1),]
ad.col <- grep('dti_jlf_ad', names(ad.data))
male.ad.values <- scale(male.ad.data[,ad.col])[,1:length(ad.col)]
male.ad.outcome <- scale(male.ad.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ad.outcome <- male.ad.outcome[complete.cases(male.ad.data[,ad.col])]
male.ad.values <- male.ad.values[complete.cases(male.ad.data[,ad.col]),]
# Now run the variable selection
maleAdBetaMatrix <- runLassoforHiLo(male.ad.values, male.ad.outcome, nCor=30,alphaSequence=.5)
maleAdValues <- rmFat(maleAdBetaMatrix, male.ad.values, .95)
maleAdValues <- regressWithinModality(maleAdValues, 'dti_jlf_ad')
maleAdFitStats <- computeModelFitMetrics(returnBetas=T,x = maleAdValues, y= male.ad.outcome)

# Now do female
female.ad.data <- ad.data[which(ad.data$sex==2),]
female.ad.values <- scale(female.ad.data[,ad.col])[,1:length(ad.col)]
female.ad.outcome <- scale(female.ad.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleAdBetaMatrix <- runLassoforHiLo(female.ad.values, female.ad.outcome, nCor=30,alphaSequence=.5)
femaleAdValues <- rmFat(femaleAdBetaMatrix, female.ad.values, .95)
femaleAdValues <- regressWithinModality(femaleAdValues, 'dti_jlf_ad')
femaleAdFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleAdValues, y= female.ad.outcome)

# Now onto RD
male.rd.data <- rd.data[which(rd.data$sex==1),]
rd.col <- grep('dti_jlf_rd', names(rd.data))
male.rd.values <- scale(male.rd.data[,rd.col])[,1:length(rd.col)]
male.rd.outcome <- scale(male.rd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.rd.outcome <- male.rd.outcome[complete.cases(male.rd.data[,rd.col])]
male.rd.values <- male.rd.values[complete.cases(male.rd.data[,rd.col]),]
# Now run the variable selection
maleRdBetaMatrix <- runLassoforHiLo(male.rd.values, male.rd.outcome, nCor=30,alphaSequence=.5)
maleRdValues <- rmFat(maleRdBetaMatrix, male.rd.values, .95)
maleRdValues <- regressWithinModality(maleRdValues, 'dti_jlf_rd')
maleRdFitStats <- computeModelFitMetrics(returnBetas=T,x = maleRdValues, y= male.rd.outcome)

# Now do female
female.rd.data <- rd.data[which(rd.data$sex==2),]
female.rd.values <- scale(female.rd.data[,rd.col])[,1:length(rd.col)]
female.rd.outcome <- scale(female.rd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.rd.outcome <- female.rd.outcome[complete.cases(female.rd.data[,rd.col])]
female.rd.values <- female.rd.values[complete.cases(female.rd.data[,rd.col]),]

# Now run the variable selection
femaleRdBetaMatrix <- runLassoforHiLo(female.rd.values, female.rd.outcome, nCor=30,alphaSequence=.5)
femaleRdValues <- rmFat(femaleRdBetaMatrix, female.rd.values, .95)
femaleRdValues <- regressWithinModality(femaleRdValues, 'dti_jlf_rd')
femaleRdFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleRdValues, y= female.rd.outcome)

# Now plot all of the beta weight selection values
maleVol <- plotSelection(maleVolBetaMatrix, 'Male Vol')
femaleVol <- plotSelection(femaleVolBetaMatrix, 'Female Vol')
maleCbf <- plotSelection(maleCbfBetaMatrix, 'Male CBF')
femaleCbf <- plotSelection(femaleCbfBetaMatrix, 'Female CBF')
maleGmd <- plotSelection(maleGmdBetaMatrix, 'Male GMD')
femaleGmd <- plotSelection(femaleGmdBetaMatrix, 'Female GMD')
maleCt <- plotSelection(maleCtBetaMatrix, 'Male CT')
femaleCt <- plotSelection(femaleCtBetaMatrix, 'Female CT')
maleReho <- plotSelection(maleRehoBetaMatrix, 'Male REHO')
femaleReho <- plotSelection(femaleRehoBetaMatrix, 'Female REHO')
maleAlff <- plotSelection(maleAlffBetaMatrix, 'Male ALFF')
femaleAlff <- plotSelection(femaleAlffBetaMatrix, 'Female ALFF')
maleTr <- plotSelection(maleTrBetaMatrix, 'Male TR')
femaleTr <- plotSelection(femaleTrBetaMatrix, 'Female TR')
maleRd <- plotSelection(maleRdBetaMatrix, 'Male Rd')
femaleRd <- plotSelection(femaleRdBetaMatrix, 'Female Rd')
maleAd <- plotSelection(maleAdBetaMatrix, 'Male Ad')
femaleAd <- plotSelection(femaleAdBetaMatrix, 'Female Ad')

pdf('selectionOutput.pdf')
maleVol
femaleVol
maleCbf
femaleCbf
maleGmd
femaleGmd
maleCt
femaleCt
maleReho
femaleReho
maleAlff
femaleAlff
maleTr
femaleTr
maleRd
femaleRd
maleAd
femaleAd
dev.off()

output <- rbind(maleVolFitStats[[1]], maleCbfFitStats[[1]], maleGmdFitStats[[1]], maleCtFitStats[[1]], maleRehoFitStats[[1]], maleAlffFitStats[[1]], maleTrFitStats[[1]], maleRdFitStats[[1]], maleAdFitStats[[1]])
tmp <- c('Vol', 'Cbf', 'Gmd', 'CT', 'Reho', 'Alff', 'TR', 'RD', 'AD')
output <- cbind(tmp, output)
write.csv(output, 'maleFitStats.csv', quote=F, row.names=F)
output <- rbind(femaleVolFitStats[[1]], femaleCbfFitStats[[1]], femaleGmdFitStats[[1]], femaleCtFitStats[[1]], femaleRehoFitStats[[1]], femaleAlffFitStats[[1]], femaleTrFitStats[[1]], femaleRdFitStats[[1]], femaleAdFitStats[[1]])
output <- cbind(tmp, output)
write.csv(output, 'femaleFitStats.csv', quote=F, row.names=F)

# Now write the beta values
output <- c(maleVolFitStats[[2]], maleCbfFitStats[[2]], maleGmdFitStats[[2]], maleCtFitStats[[2]], maleRehoFitStats[[2]], maleAlffFitStats[[2]], maleTrFitStats[[2]], maleRdFitStats[[2]], maleAdFitStats[[2]])
tmp <- c(rep('Vol', length(maleVolFitStats[[2]])), rep('CBF', length(maleCbfFitStats[[2]])), rep('GMD', length(maleGmdFitStats[[2]])), rep('CT', length(maleCtFitStats[[2]])), rep('REHO', length(maleRehoFitStats[[2]])), rep('ALFF', length(maleAlffFitStats[[2]])), rep('TR', length(maleTrFitStats[[2]])), rep('RD', length(maleRdFitStats[[2]])), rep('AD', length(maleAdFitStats[[2]])))
output <- cbind(output, tmp)
output <- output[-which(rownames(output)=='(Intercept)'),]
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'maleFitBetas.csv', quote=F, row.names=F)

# Now do female
output <- c(femaleVolFitStats[[2]], femaleCbfFitStats[[2]], femaleGmdFitStats[[2]], femaleCtFitStats[[2]], femaleRehoFitStats[[2]], femaleAlffFitStats[[2]], femaleTrFitStats[[2]], femaleRdFitStats[[2]], femaleAdFitStats[[2]])
tmp <- c(rep('Vol', length(femaleVolFitStats[[2]])), rep('CBF', length(femaleCbfFitStats[[2]])), rep('GMD', length(femaleGmdFitStats[[2]])), rep('CT', length(femaleCtFitStats[[2]])), rep('REHO', length(femaleRehoFitStats[[2]])), rep('ALFF', length(femaleAlffFitStats[[2]])), rep('TR', length(femaleTrFitStats[[2]])), rep('RD', length(femaleRdFitStats[[2]])), rep('AD', length(femaleAdFitStats[[2]])))
output <- cbind(output, tmp)
output <- output[-which(rownames(output)=='(Intercept)'),]
rownames(output) <- strSplitMatrixReturn(rownames(output), 'jlf_')[,2]
for(i in c('vol_', 'cbf_', 'gmd_', 'ct_', 'reho_', 'alff_', 'tr_', 'rd_', 'ad_')){
  rownames(output) <- gsub(x=rownames(output), pattern=i, replacement='')
}
output <- cbind(output, rownames(output))
output <- as.data.frame(output)
output <- reshape(data=output, timevar='tmp', idvar='V3', v.names='output', direction='wide')
write.csv(output, 'femaleFitBetas.csv', quote=F, row.names=F)

# Now do all modality data
tmp <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
tmp <- merge(tmp, gmd.data,  by=intersect(names(tmp), names(gmd.data)))
tmp <- merge(tmp, ct.data,  by=intersect(names(tmp), names(ct.data)))
#tmp <- merge(tmp, reho.data,  by=intersect(names(tmp), names(reho.data)))
#tmp <- merge(tmp, alff.data,  by=intersect(names(tmp), names(alff.data)))
tmp <- merge(tmp, tr.data,  by=intersect(names(tmp), names(tr.data)))
tmp <- merge(tmp, fa.data.label, by=intersect(names(tmp), names(fa.data.label)))
tmp <- merge(tmp, tr.data.label, by=intersect(names(tmp), names(tr.data.label)))
all.data <- tmp

# Now perform the analysis
male.all.data <- all.data[which(all.data$sex==1),]
all.col <- grep('jlf', names(all.data))
all.col <- append(all.col, grep('dti_dtitk_jhulabel', names(all.data)))
male.all.values <- scale(male.all.data[,all.col])[,1:length(all.col)]
male.all.outcome <- scale(male.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.all.outcome <- male.all.outcome[complete.cases(male.all.data[,all.col])]
male.all.values <- male.all.values[complete.cases(male.all.data[,all.col]),]

# Now run var selection
maleAllBetaMatrix <- runLassoforHiLo(male.all.values, male.all.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.5, 1, .05)){
  maleAllValues <- rmFat(maleAllBetaMatrix, male.all.values, val)
  maleAllFitStats <- computeModelFitMetrics(returnBetas=F,x = maleAllValues, y= male.all.outcome)
  tmpOut <- rbind(tmpOut, maleAllFitStats)
}
maleAllOut <- tmpOut
write.csv(maleAllOut, 'maleQuantCutOffAll.csv', quote=F, row.names=F)


# Now do female data
female.all.data <- all.data[which(all.data$sex==2),]
female.all.values <- scale(female.all.data[,all.col])[,1:length(all.col)]
female.all.outcome <- scale(female.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.all.outcome <- female.all.outcome[complete.cases(female.all.data[,all.col])]
female.all.values <- female.all.values[complete.cases(female.all.data[,all.col]),]

# Now run the variable selection
femaleAllBetaMatrix <- runLassoforHiLo(female.all.values, female.all.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val in seq(.5, 1, .05)){
  femaleAllValues <- rmFat(femaleAllBetaMatrix, female.all.values, val)
  femaleAllFitStats <- computeModelFitMetrics(returnBetas=F,x = femaleAllValues, y= female.all.outcome)
  tmpOut <- rbind(tmpOut, femaleAllFitStats)
}
femaleAllOut <- tmpOut
write.csv(femaleAllOut, 'femaleQuantCutOffAll.csv', quote=F, row.names=F)

# Now perform the same task with the GMD factor score
male.all.data <- all.data[which(all.data$sex==1),]
all.col <- grep('jlf', names(all.data))
all.col <- append(all.col, grep('dti_dtitk_jhulabel', names(all.data)))
male.all.values <- scale(male.all.data[,all.col])[,1:length(all.col)]
male.all.values <- male.all.values[,-grep('mprage_jlf_gmd', colnames(male.all.values))]
male.all.values <- cbind(male.all.values, male.all.data$Overall_GMD)
colnames(male.all.values)[295] <- 'Overall_GMD'
male.all.outcome <- scale(male.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.all.outcome <- male.all.outcome[complete.cases(male.all.data[,all.col])]
male.all.values <- male.all.values[complete.cases(male.all.data[,all.col]),]

# Now run var selection
maleAllBetaMatrix <- runLassoforHiLo(male.all.values, male.all.outcome, nCor=30,alphaSequence=.5)
maleAllValues <- rmFat(maleAllBetaMatrix, male.all.values)
maleAllFitStatsOverall <- computeModelFitMetrics(returnBetas=T,x = maleAllValues, y= male.all.outcome)


# Now do female data
female.all.data <- all.data[which(all.data$sex==2),]
female.all.values <- scale(female.all.data[,all.col])[,1:length(all.col)]
female.all.values <- female.all.values[,-grep('mprage_jlf_gmd', colnames(female.all.values))]
female.all.values <- cbind(female.all.values, female.all.data$Overall_GMD)
colnames(female.all.values)[295] <- 'Overall_GMD'
female.all.outcome <- scale(female.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.all.outcome <- female.all.outcome[complete.cases(female.all.data[,all.col])]
female.all.values <- female.all.values[complete.cases(female.all.data[,all.col]),]

# Now run the variable selection
femaleAllBetaMatrix <- runLassoforHiLo(female.all.values, female.all.outcome, nCor=30,alphaSequence=.5)
tmpOut <- NULL
for(val 
femaleAllValues <- rmFat(femaleAllBetaMatrix, female.all.values)
femaleAllFitStatsOverall <- computeModelFitMetrics(returnBetas=T,x = femaleAllValues, y= female.all.outcome)

# Now write the male overall values
maleOutput <- rbind(maleAllFitStats[[1]], maleAllFitStatsOverall[[1]])
write.csv(maleOutput, 'maleAllModalFitMetrics.csv', quote=F, row.names=F)
femaleOutput <- rbind(femaleAllFitStats[[1]], femaleAllFitStatsOverall[[1]])
write.csv(femaleOutput, 'femaleAllModalFitMetrics.csv', quote=F, row.names=F)


## Now perform an analysis to see if the variables selected for each modality differ across genders
## This will be performed by getting boot strapped confidence intervals for the selected regression coefficients.

## Load library(s)
install_load('boot', 'data.table')

## Now test the volume straps
male.vol.values <- as.data.frame(male.vol.values)
male.vol.values$F1_Exec_Comp_Cog_Accuracy <- male.vol.outcome
female.vol.values <- as.data.frame(female.vol.values)
female.vol.values$F1_Exec_Comp_Cog_Accuracy <- female.vol.outcome
volFormula <- returnFullModel(maleVolFitStats, femaleVolFitStats)
resultsMale <- boot(data=male.vol.values, statistic=bs, R=10000, formula=volFormula)
resultsFemale <- boot(data=female.vol.values, statistic=bs, R=10000, formula=volFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(volFormula, '~')[3]), 'mprage_jlf_vol_')
sigVals <- NULL
pdf('volStrap.pdf')
for(i in 1:dim(diffValues)[2]){
  hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
  abline(v=mean(diffValues[,i]))
  stdVal <- stand_err(diffValues[,i])
  lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
  upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
  abline(v=lowerVal)
  abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigVolVals.csv', quote=F)

## Now do CBF
male.cbf.values <- as.data.frame(male.cbf.values)
male.cbf.values$F1_Exec_Comp_Cog_Accuracy <- male.cbf.outcome
female.cbf.values <- as.data.frame(female.cbf.values)
female.cbf.values$F1_Exec_Comp_Cog_Accuracy <- female.cbf.outcome
cbfFormula <- returnFullModel(maleCbfFitStats, femaleCbfFitStats)
resultsMale <- boot(data=male.cbf.values, statistic=bs, R=10000, formula=cbfFormula)
resultsFemale <- boot(data=female.cbf.values, statistic=bs, R=10000, formula=cbfFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(cbfFormula, '~')[3]), 'pcasl_jlf_cbf_')
sigVals <- NULL
pdf('cbfStrap.pdf')
for(i in 1:dim(diffValues)[2]){
    hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
    abline(v=mean(diffValues[,i]))
    stdVal <- stand_err(diffValues[,i])
    lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
    upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
    abline(v=lowerVal)
    abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigCbfVals.csv', quote=F)

## Now do GMD
male.gmd.values <- as.data.frame(male.gmd.values)
male.gmd.values$F1_Exec_Comp_Cog_Accuracy <- male.gmd.outcome
female.gmd.values <- as.data.frame(female.gmd.values)
female.gmd.values$F1_Exec_Comp_Cog_Accuracy <- female.gmd.outcome
gmdFormula <- returnFullModel(maleGmdFitStats, femaleGmdFitStats)
resultsMale <- boot(data=male.gmd.values, statistic=bs, R=10000, formula=gmdFormula)
resultsFemale <- boot(data=female.gmd.values, statistic=bs, R=10000, formula=gmdFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(gmdFormula, '~')[3]), 'mprage_jlf_gmd_')
sigVals <- NULL
pdf('gmdStrap.pdf')
for(i in 1:dim(diffValues)[2]){
    hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
    abline(v=mean(diffValues[,i]))
    stdVal <- stand_err(diffValues[,i])
    lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
    upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
    abline(v=lowerVal)
    abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigGmdVals.csv', quote=F)

## Now do CT
male.ct.values <- as.data.frame(male.ct.values)
male.ct.values$F1_Exec_Comp_Cog_Accuracy <- male.ct.outcome
female.ct.values <- as.data.frame(female.ct.values)
female.ct.values$F1_Exec_Comp_Cog_Accuracy <- female.ct.outcome
ctFormula <- returnFullModel(maleCtFitStats, femaleCtFitStats)
resultsMale <- boot(data=male.ct.values, statistic=bs, R=10000, formula=ctFormula)
resultsFemale <- boot(data=female.ct.values, statistic=bs, R=10000, formula=ctFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(ctFormula, '~')[3]), 'mprage_jlf_ct_')
sigVals <- NULL
pdf('ctStrap.pdf')
for(i in 1:dim(diffValues)[2]){
    hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
    abline(v=mean(diffValues[,i]))
    stdVal <- stand_err(diffValues[,i])
    lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
    upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
    abline(v=lowerVal)
    abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigCtVals.csv', quote=F)

## Now do Reho
male.reho.values <- as.data.frame(male.reho.values)
male.reho.values$F1_Exec_Comp_Cog_Accuracy <- male.reho.outcome
female.reho.values <- as.data.frame(female.reho.values)
female.reho.values$F1_Exec_Comp_Cog_Accuracy <- female.reho.outcome
rehoFormula <- returnFullModel(maleRehoFitStats, femaleRehoFitStats)
resultsMale <- boot(data=male.reho.values, statistic=bs, R=10000, formula=rehoFormula)
resultsFemale <- boot(data=female.reho.values, statistic=bs, R=10000, formula=rehoFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(rehoFormula, '~')[3]), 'rest_jlf_reho_')
sigVals <- NULL
pdf('rehoStrap.pdf')
for(i in 1:dim(diffValues)[2]){
    hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
    abline(v=mean(diffValues[,i]))
    stdVal <- stand_err(diffValues[,i])
    lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
    upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
    abline(v=lowerVal)
    abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigRehoVals.csv', quote=F)

## Now do ALFF
male.alff.values <- as.data.frame(male.alff.values)
male.alff.values$F1_Exec_Comp_Cog_Accuracy <- male.alff.outcome
female.alff.values <- as.data.frame(female.alff.values)
female.alff.values$F1_Exec_Comp_Cog_Accuracy <- female.alff.outcome
alffFormula <- returnFullModel(maleAlffFitStats, femaleAlffFitStats)
resultsMale <- boot(data=male.alff.values, statistic=bs, R=10000, formula=alffFormula)
resultsFemale <- boot(data=female.alff.values, statistic=bs, R=10000, formula=alffFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(alffFormula, '~')[3]), 'rest_jlf_alff_')
sigVals <- NULL
pdf('alffStrap.pdf')
for(i in 1:dim(diffValues)[2]){
    hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
    abline(v=mean(diffValues[,i]))
    stdVal <- stand_err(diffValues[,i])
    lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
    upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
    abline(v=lowerVal)
    abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigAlffVals.csv', quote=F)

## Now do Tr
male.tr.values <- as.data.frame(male.tr.values)
male.tr.values$F1_Exec_Comp_Cog_Accuracy <- male.tr.outcome
female.tr.values <- as.data.frame(female.tr.values)
female.tr.values$F1_Exec_Comp_Cog_Accuracy <- female.tr.outcome
trFormula <- returnFullModel(maleTrFitStats, femaleTrFitStats)
resultsMale <- boot(data=male.tr.values, statistic=bs, R=10000, formula=trFormula)
resultsFemale <- boot(data=female.tr.values, statistic=bs, R=10000, formula=trFormula)
# Now find the difference across folds
diffValues <- resultsMale$t-resultsFemale$t
# Now get a mn and a max value for the histograms
minVal <- floor(min(diffValues))
maxVal <- ceiling(max(diffValues))
# Now produce the histograms
roiNames <- strSplitMatrixReturn(as.character(strSplitMatrixReturn(trFormula, '~')[3]), 'rest_jlf_tr_')
sigVals <- NULL
pdf('trStrap.pdf')
for(i in 1:dim(diffValues)[2]){
    hist(diffValues[,i], main=roiNames[i], xlim=c(-1, 1), ylim=c(0,3000))
    abline(v=mean(diffValues[,i]))
    stdVal <- stand_err(diffValues[,i])
    lowerVal <- mean(diffValues[,i]) - (1.96*stdVal)
    upperVal <- mean(diffValues[,i]) + (1.96*stdVal)
    abline(v=lowerVal)
    abline(v=upperVal)
  if(between(0, lowerVal, upperVal)=='FALSE'){
    sigVals <- append(sigVals, roiNames[i])
  }
}
dev.off()
write.csv(sigVals, 'sigTrVals.csv', quote=F)
