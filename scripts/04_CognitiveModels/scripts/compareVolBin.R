source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')

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

# Now prepare the all data values 
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))

# Now scale our overall metrics 
all.data$mprage_jlf_vol_ICV <- scale(all.data$mprage_jlf_vol_ICV)
all.data$pcasl_jlf_cbf_MeanGM <- scale(all.data$pcasl_jlf_cbf_MeanGM)
all.data$mprage_jlf_gmd_MeanGMD <- sclae(all.data$mprage_jlf_gmd_MeanGMD)
all.data$dti_jlf_tr_MeanTR <- scale(tr.data$dti_jlf_tr_MeanTR)

# Now declare the formula we are going to use to predict performance based on all metrics
formVal <- as.formula(F1_Exec_Comp_Cog_Accuracy ~ mprage_jlf_vol_ICV + dti_jlf_tr_MeanTR + pcasl_jlf_cbf_MeanGM + mprage_jlf_gmd_MeanGMD)

# Now build a model in all male data 
m1 <- lm(formVal, data=all.data[which(all.data$sex==1),])
m2 <- lm(formVal, data=all.data[which(all.data$sex==2),])

all.data.freeze <- all.data
all.data <- all.data[which(all.data$sex==1),]

# Nowget the bin's
all.data$volBin <- NA
all.data$volBin[which(all.data$mprage_jlf_vol_ICV < quantile(all.data$mprage_jlf_vol_ICV, probs=c(0,.33, .66, 1))[2])] <- 'LO'
all.data$volBin[which(all.data$mprage_jlf_vol_ICV > quantile(all.data$mprage_jlf_vol_ICV, probs=c(0,.33, .66, 1))[3])] <- 'HI'
all.data$volBin[all.data$volBin==NA] <- 'ME'

# Now build our models
m3 <- lm(formVal, data=all.data[which(all.data$volBin=='HI'),])
m4 <- lm(formVal, data=all.data[which(all.data$volBin=='LO'),])

# Now do female values
all.data <- all.data.freeze
all.data <- all.data[which(all.data$sex==2),]

# Now get the bin's
all.data$volBin <- NA
all.data$volBin[which(all.data$mprage_jlf_vol_ICV < quantile(all.data$mprage_jlf_vol_ICV, probs=c(0,.33, .66, 1))[2])] <- 'LO'
all.data$volBin[which(all.data$mprage_jlf_vol_ICV > quantile(all.data$mprage_jlf_vol_ICV, probs=c(0,.33, .66, 1))[3])] <- 'HI'
all.data$volBin[all.data$volBin==NA] <- 'ME'

# Now build our models
m5 <- lm(formVal, data=all.data[which(all.data$volBin=='HI'),])
m6 <- lm(formVal, data=all.data[which(all.data$volBin=='LO'),])

output <- cbind(coef(m1),coef(m2),coef(m3),coef(m4),coef(m5),coef(m6))
colnames(output) <- c('All Male', 'All Female', 'Hi Male', 'Lo Male', 'Hi Female', 'Lo Female')
write.csv(output, 'summaryMetricBetaByVolBin.csv', quote=F)
