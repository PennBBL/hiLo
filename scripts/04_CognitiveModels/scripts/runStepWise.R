# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'MASS','SignifReg')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
fa.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabelsData.csv')
tr.data.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuTRLabelsData.csv')

# Now loop through all of our modalities, data frames, and genders and prep our output
genderVals <- c(1,2)
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'ct.data', 'cc.data', 'reho.data', 'alff.data', 'tr.data', 'fa.data.label')
dataGrepNames <- c('mprage_jlf_vol', 'pcasl_jlf_cbf', 'mprage_jlf_gmd', 'mprage_jlf_ct', 'mprage_jlf_cortcon', 'rest_jlf_reho', 'rest_jlf_alff','dti_jlf_tr', 'dti_dtitk_jhulabel_fa')

# Now create our loops
allOut <- NA
for(w in seq(.95, .95, .05)){
  for(g in genderVals){
    for(z in 1:length(dataNames)){
      vals <- returnCVStepFit(dataFrame=get(dataNames[z]), grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=1000, nCor=30, selectionPercent=w)
      tmp <- cbind(g, dataNames[z], dataGrepNames[z], vals[[1]])
      tmp2 <- vals[[2]]
      write.csv(tmp2, paste(g,z,'BetaVals.csv', sep=''), quote=F, row.names=T)
      allOut <- rbind(allOut, tmp)
    }
  }
  write.csv(allOut, paste(w,'stepIndividualModal.csv', sep=''), quote=F, row.names=F)
}
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

maleAll <- returnCVStepFit(all.data, 1, 'jlf', iterationCount=1000, nCor=30, selectionPercent=.9)
write.csv(maleAll[[2]], 'maleAllBetas.csv', quote=F, row.names=T)
femaleAll <-  returnCVStepFit(all.data, 2, 'jlf', iterationCount=1000, nCor=30, selectionPercent=.9)
write.csv(femaleAll[[2]], 'femaleAllBetas.csv', quote=F, row.names=T)
allMOut <- rbind(maleAll[[1]], femaleAll[[1]])
write.csv(allMOut, 'stepAllModal.csv', quote=F, row.names=F)

# Now produce the selection percentages
# Now create our loops
for(g in genderVals){
  for(z in 1:length(dataNames)){
    vals <- returnSelection(dataFrame=get(dataNames[z]), grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=1000, nCor=30, selectionPercent=.9)
    vals <- (rowSums(vals)/1000)[-1]
    write.csv(vals, paste(g,z,'SelectionVals.csv', sep=''), quote=F, row.names=T)
  }
}


# Tmp obtain the number of factors for each modality by sex
install_load('psych', 'nFactors', 'corrplot')
eValues <- eigen(polychoric(t(vals))$rho)$values
plotnScree(nScree(eValues, model="factors"), main="Scree Plot & Parallel Analysis")

# Now extract 28 factors
xcor <- polychoric(t(vals))$rho
mod <- fa(xcor, 28, rotate='promax')
mod.sort <- fa.sort(mod)
