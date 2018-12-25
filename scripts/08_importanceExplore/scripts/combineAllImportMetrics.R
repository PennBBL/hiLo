## This script will be used to compare all of the impotance metrics for the hi lo project

## Load library(s)
install_load('ggplot2')
## The first thing we have to do is produce all of the importance metrics
## Little bash fandangling
system("for i in `ls prep*R` ; do Rscript ${i} ; done")

## Now we will read all of the male values and produce the correlation matrix
randImp <- read.csv('./randForImpMale.csv')
colnames(randImp) <- c("ROI_readable", "randomForestImp", "modality", "ModelingTechniqueRF")
randImp <- randImp[-which(randImp$modality=='all.data'),]
randImp <- randImp[,-c(3,4)]
relifImp <- read.csv('./reliefFImpMale.csv')
colnames(relifImp) <- c("ROI_readable", "reliefImp", "modality", "ModelingTechniqueRF")
relifImp <- relifImp[-which(relifImp$modality=='all.data'),]
relifImp <- relifImp[,-c(3,4)]
ridgeImp <- read.csv('./ridgeImpMale.csv')
colnames(ridgeImp) <- c("ROI_readable", "ridgeImp", "modality", "ModelingTechniqueRidge")
ridgeImp <- ridgeImp[-which(ridgeImp$modality=='all.data'),]
ridgeImp <- ridgeImp[,-c(3,4)]
sregImp <- read.csv('selfRegImpMale.csv')
sregImp[,1] <- gsub(sregImp[,1], pattern='tmpDatX', replacement='')
colnames(sregImp) <- c("ROI_readable", "srelfRegImp", "modality", "ModelingTechniqueSR")
sregImp <- sregImp[-which(sregImp$modality=='all.data'),]
sregImp <- sregImp[,-c(3,4)]
efImp <- read.csv('./effSizeImp.csv')

## Now isolate the male eff size variables
efImp <- efImp[which(efImp$sex=='M'),]

## Now combine all of these
all.data <- merge(efImp, randImp)
all.data <- merge(all.data, relifImp, by='ROI_readable')
all.data <- merge(all.data, ridgeImp, by='ROI_readable')
all.data <- merge(all.data, sregImp, by='ROI_readable')
