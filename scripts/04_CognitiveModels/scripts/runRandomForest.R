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
maleVol <- randomForest(x=male.vol.values[index,], y=male.vol.outcome[index], xtest=male.vol.values[-index,],ytest=male.vol.outcome[-index], importance=T,keep.forest=T, ntree=800, proximity=T)

