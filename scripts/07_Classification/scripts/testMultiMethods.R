# Load library(s)
install_load('caret', 'foreign', 'nnet', 'ggplot2', 'reshape2', 'MASS', 'Hmisc', 'adabag')

# Load data
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

all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, cc.data, by=intersect(names(all.data), names(cc.data)))
all.data <- merge(all.data, ct.data, by=intersect(names(all.data), names(ct.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))
all.data <- merge(all.data, fa.data)
all.data <- merge(all.data, rd.data)
all.data <- merge(all.data, ad.data)

# Declare any functions required
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.34,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 3
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 2
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 1
  return(data)
}

# Add the tertile groups as the ground truth
vol.data.male <- vol.data[which(vol.data$sex==1),]
vol.data.male <- returnPerfBin(vol.data.male)
# Now see what a kmeans can do for vol data 
tmp <- kmeans(x=vol.data.male$mprage_jlf_vol_ICV, centers=3)
tmp1 <- kmeans(x=scale(vol.data.male[,grep('_jlf_', names(vol.data.male))]), centers=3)
vol.data.male$perfBin <- factor(vol.data.male$perfBin)
m1 <- polr(perfBin ~ mprage_jlf_vol_ICV, data=vol.data.male)
# Now explore bagging
tmpDat <- vol.data.male[,c(grep('perfBin', names(vol.data.male)), grep('_jlf_', names(vol.data.male)))]
m2 <- bagging.cv(perfBin ~ ., tmpDat)
tmpDat <- all.data.male[,c(grep('perfBin', names(all.data.male)), grep('_jlf_', names(all.data.male)))]
tmpDat <- all.data.male[,c(grep('F1_Exec_Comp_Cog_Accuracy', names(all.data.male)), grep('_jlf_', names(all.data.male)))]
m2 <- boosting(F1_Exec_Comp_Cog_Accuracy ~ ., tmpDat)


# Now try bagging with all male subjects
all.data.male <- all.data[which(all.data$sex==1),]
all.data.male <- returnPerfBin(all.data.male)
tmpDat <- all.data.male[,c(grep('perfBin', names(all.data.male)), grep('_jlf_', names(all.data.male)))]
tmpDat$perfBin <- as.factor(tmpDat$perfBin)
m3 <- bagging.cv(perfBin ~ ., tmpDat)#, boos = TRUE, mfinal = 10, control = (minsplit = 0))
m4 <- boosting.cv(perfBin ~ ., tmpDat)

# Now explore boosting with a 80% 20% cv method
index <- createFolds(tmpDat$perfBin, k=5, list=T, returnTrain=T)[[1]]
tmpDatTrain <- tmpDat[index,]
tmpDatValid <- tmpDat[-index,]
m5 <- boosting(perfBin ~ ., tmpDatTrain)
outPred <- predict(m5, tmpDatValid)
