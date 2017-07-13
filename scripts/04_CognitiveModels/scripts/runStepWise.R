# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'MASS','SignifReg')

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

# Now loop through all of our modalities, data frames, and genders and prep our output
genderVals <- c(1,2)
dataNames <- c('vol.data', 'cbf.data', 'gmd.data', 'ct.data', 'reho.data', 'alff.data', 'tr.data', 'fa.data.label')
dataGrepNames <- c('mprage_jlf_vol', 'pcasl_jlf_cbf', 'mprage_jlf_gmd', 'mprage_jlf_ct', 'rest_jlf_reho', 'rest_jlf_alff','dti_jlf_tr', 'dti_dtitk_jhulabel_fa')

# Now create our loops
allOut <- NA
for(g in genderVals){
  for(z in 1:length(dataNames)){
    vals <- returnCVStepFit(dataFrame=get(dataNames[z]), grepID=dataGrepNames[z], genderID=g, pValue=.05, iterationCount=1000, nCor=30, selectionPercent=.75)
    tmp <- cbind(g, dataNames[z], dataGrepNames[z], vals[[1]])
    tmp2 <- vals[[2]]
    write.csv(tmp2, paste(g,z,'BetaVals.csv', sep=''), quote=F, row.names=T)
    allOut <- rbind(allOut, tmp)
  }
}
write.csv(allOut, 'stepIndividualModal.csv', quote=F, row.names=F)
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

maleAll <- returnCVStepFit(all.data, 1, 'jlf')
femaleAll <-  returnCVStepFit(all.data, 2, 'jlf')
allMOut <- rbind(maleAll, femaleAll)
write.csv(allMOut, 'stepAllModal.csv', quote=F, row.names=F)
