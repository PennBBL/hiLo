# AFGR September 12 2016
# This script is going to be used to run the variable selection for the hi lo paper
# It will perform this for each modality individually and then all of them together
# It is important to also note that this is performed within sex

# First thing we need to do is load our library(s)
source('/home/adrose/varSelectionHiLo/scripts/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfTRData.csv')

## Declare function(s)
doEverything <- function(dataFrame, genderValue, grepPattern, cutOffValue){
   # First isolate to gender of interest
   dataFrame.isolated <- dataFrame[which(dataFrame$sex==genderValue),]
   # Now find columns of interest
   colsOfInterest <- grep(grepPattern, names(dataFrame))
   valuesToUse <- scale(dataFrame.isolated[,colsOfInterest])[,1:length(colsOfInterest)]
   # Now create the output
   outcomeOfInterest <- scale(dataFrame.isolated$F1_Exec_Comp_Cog_Accuracy)[,1]
   outcomeOfInterest <- outcomeOfInterest[complete.cases(valuesToUse)]
   valuesToUse <- valuesToUse[complete.cases(valuesToUse),]
   # Now run variable selection
   betaMatrix <- runLassoforHiLo(valuesToUse, outcomeOfInterest, nCor=10)
   passThreshold <- rmFat(betaMatrix, valuesToUse, cutOffValue)
   fitStats <- computeModelFitMetrics(returnBetas=T,x = passThreshold, y = outcomeOfInterest)
   return(fitStats)
}
# Now grab the fit stats for each modality
#vol.male <- doEverything(vol.data, 1, 'mprage_jlf_vol', 25)
#vol.female <- doEverything(vol.data, 2, 'mprage_jlf_vol', 25)
#cbf.male <- doEverything(cbf.data, 1, 'pcasl_jlf_cbf', 25)
#cbf.female <- doEverything(cbf.data, 2, 'pcasl_jlf_cbf', 25)
#gmd.male <- doEverything(gmd.data, 1, 'mprage_jlf_gmd', 25)
#gmd.female <- doEverything(gmd.data, 2, 'mprage_jlf_gmd', 25)
#ct.male <- doEverything(ct.data, 1, 'mprage_jlf_ct', 5)
#ct.female <- doEverything(ct.data, 2, 'mprage_jlf_ct', 5)
#reho.male <- doEverything(reho.data, 1, 'rest_jlf_reho', 5)
#reho.female <- doEverything(reho.data, 2, 'rest_jlf_reho', 5)
#alff.male <- doEverything(alff.data, 1, 'rest_jlf_alff', 5)
#alff.female <- doEverything(alff.data, 2, 'rest_jlf_alff', 5)
#ad.male <- doEverything(ad.data, 1, 'dti_jlf_ad', 5)
#ad.female <- doEverything(ad.data, 2, 'dti_jlf_ad', 5)
#fa.male <- doEverything(fa.data, 1, 'dti_jlf_fa', 5)
#fa.female <- doEverything(fa.data, 2, 'dti_jlf_fa', 5)
#rd.male <- doEverything(rd.data, 1, 'dti_jlf_rd', 5)
#rd.female <- doEverything(rd.data, 2, 'dti_jlf_rd', 5)
#tr.male <- doEverything(tr.data, 1, 'dti_jlf_tr', 5)
#tr.female <- doEverything(tr.data, 2, 'dti_jlf_tr', 5)

#output.male <- rbind(vol.male, cbf.male, gmd.male, ct.male, reho.male, alff.male, ad.male, fa.male, rd.male, tr.male)
#output.female <- rbind(vol.female, cbf.female, gmd.female, ct.female, reho.female, alff.female, ad.female, fa.female, rd.female, tr.female)

## Lets do volume data frist
# start with male data
male.vol.data <- vol.data[which(vol.data$sex==1),]
vol.col <- grep('mprage_jlf_vol', names(vol.data))
male.vol.values <- scale(male.vol.data[,vol.col])[,1:length(vol.col)]
male.vol.outcome <- scale(male.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.vol.outcome <- male.vol.outcome[complete.cases(male.vol.data[,vol.col])]
male.vol.values <- male.vol.values[complete.cases(male.vol.data[,vol.col]),]
# Now run the variable selection
maleVolBetaMatrix <- runLassoforHiLo(male.vol.values, male.vol.outcome, nCor=30)
maleVolValues <- rmFat(maleVolBetaMatrix, male.vol.values)
maleVolFitStats <- computeModelFitMetrics(returnBetas=T,x = maleVolValues, y= male.vol.outcome)

# Now do female
female.vol.data <- vol.data[which(vol.data$sex==2),]
female.vol.values <- scale(female.vol.data[,vol.col])[,1:length(vol.col)]
female.vol.outcome <- scale(female.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleVolBetaMatrix <- runLassoforHiLo(female.vol.values, female.vol.outcome, nCor=30)
femaleVolValues <- rmFat(femaleVolBetaMatrix, female.vol.values)
femaleVolFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleVolValues, y= female.vol.outcome)

## Now do CBF
# start with male
male.cbf.data <- cbf.data[which(cbf.data$sex==1),]
cbf.col <- grep('pcasl_jlf_cbf', names(male.cbf.data))
male.cbf.values <- scale(male.cbf.data[,cbf.col])[,1:length(cbf.col)]
male.cbf.outcome <- scale(male.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.cbf.outcome <- male.cbf.outcome[complete.cases(male.cbf.data[,cbf.col])]
male.cbf.values <- male.cbf.values[complete.cases(male.cbf.data[,cbf.col]),]

# Now run the variable selection 
maleCbfBetaMatrix <- runLassoforHiLo(male.cbf.values, male.cbf.outcome, nCor=25)
maleCbfValues <- rmFat(maleCbfBetaMatrix, male.cbf.values)
maleCbfFitStats <- computeModelFitMetrics(returnBetas=T,x = maleCbfValues, y= male.cbf.outcome)

# Now do female
female.cbf.data <- cbf.data[which(cbf.data$sex==2),]
female.cbf.values <- scale(female.cbf.data[,cbf.col])[,1:length(cbf.col)]
female.cbf.outcome <- scale(female.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.cbf.outcome <- female.cbf.outcome[complete.cases(female.cbf.data[,cbf.col])]
female.cbf.values <- female.cbf.values[complete.cases(female.cbf.data[,cbf.col]),]

# Now run the variable selection 
femaleCbfBetaMatrix <- runLassoforHiLo(female.cbf.values, female.cbf.outcome, nCor=25)
femaleCbfValues <- rmFat(femaleCbfBetaMatrix, female.cbf.values)
femaleCbfFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleCbfValues, y= female.cbf.outcome)

## Now do GMD
male.gmd.data <- gmd.data[which(gmd.data$sex==1),]
gmd.col <- grep('mprage_jlf_gmd', names(male.gmd.data))
male.gmd.values <- scale(male.gmd.data[,gmd.col])[,1:length(gmd.col)]
male.gmd.outcome <- scale(male.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.gmd.outcome <- male.gmd.outcome[complete.cases(male.gmd.data[,gmd.col])]
male.gmd.values <- male.gmd.values[complete.cases(male.gmd.data[,gmd.col]),]

# Now run the variable selection 
maleGmdBetaMatrix <- runLassoforHiLo(male.gmd.values, male.gmd.outcome, nCor=25)
maleGmdValues <- rmFat(maleGmdBetaMatrix, male.gmd.values)
maleGmdFitStats <- computeModelFitMetrics(returnBetas=T,x = maleGmdValues, y= male.gmd.outcome)

# Now do female
female.gmd.data <- gmd.data[which(gmd.data$sex==2),]
female.gmd.values <- scale(female.gmd.data[,gmd.col])[,1:length(gmd.col)]
female.gmd.outcome <- scale(female.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.gmd.outcome <- female.gmd.outcome[complete.cases(female.gmd.data[,gmd.col])]
female.gmd.values <- female.gmd.values[complete.cases(female.gmd.data[,gmd.col]),]

femaleGmdBetaMatrix <- runLassoforHiLo(female.gmd.values, female.gmd.outcome, nCor=25)
femaleGmdValues <- rmFat(femaleGmdBetaMatrix, female.gmd.values)
femaleGmdFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleGmdValues, y= female.gmd.outcome)

## Now on to CT
male.ct.data <- ct.data[which(ct.data$sex==1),]
ct.col <- grep('mprage_jlf_ct', names(male.ct.data))
male.ct.values <- scale(male.ct.data[,ct.col])[,1:length(ct.col)]
male.ct.outcome <- scale(male.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ct.outcome <- male.ct.outcome[complete.cases(male.ct.data[,ct.col])]
male.ct.values <- male.ct.values[complete.cases(male.ct.data[,ct.col]),]

# Now run the variable selection 
maleCtBetaMatrix <- runLassoforHiLo(male.ct.values, male.ct.outcome, nCor=25)
maleCtValues <- rmFat(maleCtBetaMatrix, male.ct.values)
maleCtFitStats <- computeModelFitMetrics(returnBetas=T,x = maleCtValues, y= male.ct.outcome)

# And now onto female
female.ct.data <- ct.data[which(ct.data$sex==2),]
female.ct.values <- scale(female.ct.data[,ct.col])[,1:length(ct.col)]
female.ct.outcome <- scale(female.ct.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.ct.outcome <- female.ct.outcome[complete.cases(female.ct.data[,ct.col])]
female.ct.values <- female.ct.values[complete.cases(female.ct.data[,ct.col]),]

femaleCtBetaMatrix <- runLassoforHiLo(female.ct.values, female.ct.outcome, nCor=25)
femaleCtValues <- rmFat(femaleCtBetaMatrix, female.ct.values)
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
maleRehoBetaMatrix <- runLassoforHiLo(male.reho.values, male.reho.outcome, nCor=30)
maleRehoValues <- rmFat(maleRehoBetaMatrix, male.reho.values)
maleRehoFitStats <- computeModelFitMetrics(returnBetas=T,x = maleRehoValues, y= male.reho.outcome)

# Now do female
female.reho.data <- reho.data[which(reho.data$sex==2),]
female.reho.values <- scale(female.reho.data[,reho.col])[,1:length(reho.col)]
female.reho.outcome <- scale(female.reho.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.reho.outcome <- female.reho.outcome[complete.cases(female.reho.data[,reho.col])]
female.reho.values <- female.reho.values[complete.cases(female.reho.data[,reho.col]),]

# Now run the variable selection
femaleRehoBetaMatrix <- runLassoforHiLo(female.reho.values, female.reho.outcome, nCor=30)
femaleRehoValues <- rmFat(femaleRehoBetaMatrix, female.reho.values)
femaleRehoFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleRehoValues, y= female.reho.outcome)

## Now onto alff
male.alff.data <- alff.data[which(alff.data$sex==1),]
alff.col <- grep('rest_jlf_alff', names(alff.data))
male.alff.values <- scale(male.alff.data[,alff.col])[,1:length(alff.col)]
male.alff.outcome <- scale(male.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.alff.outcome <- male.alff.outcome[complete.cases(male.alff.data[,alff.col])]
male.alff.values <- male.alff.values[complete.cases(male.alff.data[,alff.col]),]
# Now run the variable selection
maleAlffBetaMatrix <- runLassoforHiLo(male.alff.values, male.alff.outcome, nCor=30)
maleAlffValues <- rmFat(maleAlffBetaMatrix, male.alff.values)
maleAlffFitStats <- computeModelFitMetrics(returnBetas=T,x = maleAlffValues, y= male.alff.outcome)

# Now do female
female.alff.data <- alff.data[which(alff.data$sex==2),]
female.alff.values <- scale(female.alff.data[,alff.col])[,1:length(alff.col)]
female.alff.outcome <- scale(female.alff.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.alff.outcome <- female.alff.outcome[complete.cases(female.alff.data[,alff.col])]
female.alff.values <- female.alff.values[complete.cases(female.alff.data[,alff.col]),]

# Now run the variable selection
femaleAlffBetaMatrix <- runLassoforHiLo(female.alff.values, female.alff.outcome, nCor=30)
femaleAlffValues <- rmFat(femaleAlffBetaMatrix, female.alff.values)
femaleAlffFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleAlffValues, y= female.alff.outcome)

# Now do TR
male.tr.data <- tr.data[which(tr.data$sex==1),]
tr.col <- grep('dti_jlf_tr', names(tr.data))
male.tr.values <- scale(male.tr.data[,tr.col])[,1:length(tr.col)]
male.tr.outcome <- scale(male.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.tr.outcome <- male.tr.outcome[complete.cases(male.tr.data[,tr.col])]
male.tr.values <- male.tr.values[complete.cases(male.tr.data[,tr.col]),]
# Now run the variable selection
maleTrBetaMatrix <- runLassoforHiLo(male.tr.values, male.tr.outcome, nCor=30)
maleTrValues <- rmFat(maleTrBetaMatrix, male.tr.values)
maleTrFitStats <- computeModelFitMetrics(returnBetas=T,x = maleTrValues, y= male.tr.outcome)

# Now do female
female.tr.data <- tr.data[which(tr.data$sex==2),]
female.tr.values <- scale(female.tr.data[,tr.col])[,1:length(tr.col)]
female.tr.outcome <- scale(female.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleTrBetaMatrix <- runLassoforHiLo(female.tr.values, female.tr.outcome, nCor=30)
femaleTrValues <- rmFat(femaleTrBetaMatrix, female.tr.values)
femaleTrFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleTrValues, y= female.tr.outcome)

# Now do AD
male.ad.data <- ad.data[which(ad.data$sex==1),]
ad.col <- grep('dti_jlf_ad', names(ad.data))
male.ad.values <- scale(male.ad.data[,ad.col])[,1:length(ad.col)]
male.ad.outcome <- scale(male.ad.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.ad.outcome <- male.ad.outcome[complete.cases(male.ad.data[,ad.col])]
male.ad.values <- male.ad.values[complete.cases(male.ad.data[,ad.col]),]
# Now run the variable selection
maleAdBetaMatrix <- runLassoforHiLo(male.ad.values, male.ad.outcome, nCor=30)
maleAdValues <- rmFat(maleAdBetaMatrix, male.ad.values)
maleAdFitStats <- computeModelFitMetrics(returnBetas=T,x = maleAdValues, y= male.ad.outcome)

# Now do female
female.ad.data <- ad.data[which(ad.data$sex==2),]
female.ad.values <- scale(female.ad.data[,ad.col])[,1:length(ad.col)]
female.ad.outcome <- scale(female.ad.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleAdBetaMatrix <- runLassoforHiLo(female.ad.values, female.ad.outcome, nCor=30)
femaleAdValues <- rmFat(femaleAdBetaMatrix, female.ad.values)
femaleAdFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleAdValues, y= female.ad.outcome)

# Now onto RD
male.rd.data <- rd.data[which(rd.data$sex==1),]
rd.col <- grep('dti_jlf_rd', names(rd.data))
male.rd.values <- scale(male.rd.data[,rd.col])[,1:length(rd.col)]
male.rd.outcome <- scale(male.rd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.rd.outcome <- male.rd.outcome[complete.cases(male.rd.data[,rd.col])]
male.rd.values <- male.rd.values[complete.cases(male.rd.data[,rd.col]),]
# Now run the variable selection
maleRdBetaMatrix <- runLassoforHiLo(male.rd.values, male.rd.outcome, nCor=30)
maleRdValues <- rmFat(maleRdBetaMatrix, male.rd.values)
maleRdFitStats <- computeModelFitMetrics(returnBetas=T,x = maleRdValues, y= male.rd.outcome)

# Now do female
female.rd.data <- rd.data[which(rd.data$sex==2),]
female.rd.values <- scale(female.rd.data[,rd.col])[,1:length(rd.col)]
female.rd.outcome <- scale(female.rd.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleRdBetaMatrix <- runLassoforHiLo(female.rd.values, female.rd.outcome, nCor=30)
femaleRdValues <- rmFat(femaleRdBetaMatrix, female.rd.values)
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
tmp <- merge(vol.data, cbf.data, by=c('bblid', 'scanid'))
tmp <- merge(tmp, gmd.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, ct.data,  by=c('bblid', 'scanid'))
#tmp <- merge(tmp, reho.data,  by=c('bblid', 'scanid'))
#tmp <- merge(tmp, alff.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, ad.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, rd.data,  by=c('bblid', 'scanid'))
tmp <- merge(tmp, tr.data,  by=c('bblid', 'scanid'))
all.data <- tmp[,c(1,2, 8, 44,grep('jlf', names(tmp)))]

# Now perform the analysis
male.all.data <- all.data[which(all.data$sex.x==1),]
all.col <- grep('jlf', names(all.data))
male.all.values <- scale(male.all.data[,all.col])[,1:length(all.col)]
male.all.outcome <- scale(male.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.all.outcome <- male.all.outcome[complete.cases(male.all.data[,all.col])]
male.all.values <- male.all.values[complete.cases(male.all.data[,all.col]),]

# Now run var selection
maleAllBetaMatrix <- runLassoforHiLo(male.all.values, male.all.outcome, nCor=30)
maleAllValues <- rmFat(maleAllBetaMatrix, male.all.values)
maleAllFitStats <- computeModelFitMetrics(returnBetas=T,x = maleAllValues, y= male.all.outcome)

# Now do female data
female.all.data <- all.data[which(all.data$sex==2),]
female.all.values <- scale(female.all.data[,all.col])[,1:length(all.col)]
female.all.outcome <- scale(female.all.data$F1_Exec_Comp_Cog_Accuracy)[,1]

# Now run the variable selection
femaleAllBetaMatrix <- runLassoforHiLo(female.all.values, female.all.outcome, nCor=30)
femaleAllValues <- rmFat(femaleAllBetaMatrix, female.all.values)
femaleAllFitStats <- computeModelFitMetrics(returnBetas=T,x = femaleAllValues, y= female.all.outcome)
