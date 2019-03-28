## This script runs on Adon's MBP!
## STIR requires R version > 3.3, Galton currently runs R 3.3


## Load packages
source('~/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source("~/hiLo/scripts/01_DataPrep/functions/functions.R")
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart', 'ggrepel')

## Set a seed
set.seed(3241616)

## Load the data
vol.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
vol.data <- vol.data[,-which(names(vol.data)=='mprage_jlf_vol_ICV')]
cbf.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
alff.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
reho.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
fa.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabel.csv')
colnames(fa.data) <- gsub(x=colnames(fa.data), pattern='jhulabel', replacement='jlf')
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))
#all.data <- merge(all.data, fa.data)
all.data <- merge(all.data, alff.data)
all.data <- merge(all.data, reho.data)

## Now lets grab our reliefF importance metrics for each modality
dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','fa.data','all.data', 'reho.data', 'alff.data')
#dataNames <- c('vol.data','cbf.data','gmd.data')
#dataNames <- c('tr.data','reho.data', 'alff.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'fa', 'all.data', 'reho', 'alff')
#outName <- c('tr','reho','alff')
grepValue <- c(rep('_jlf_', 7), '_jlf_')
output.values <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- scale(as.matrix(tmpDat[,grep(grepValue[i], names(tmpDat))]))
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  ## Now train a ridge reg model
  fit.cv <- cv.glmnet(x=tmpDatX, y=tmpDatY, alpha=0,nfolds=10)
  lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
  print(lambdaVal)
  modelFit <- glmnet(x=tmpDatX, y=tmpDatY, alpha=0, lambda=lambdaVal)
  coef.vals <- coef(modelFit)[-1,]
  to.store <- cbind(rownames(cbind(coef.vals, rep(outName[i], length(coef.vals)))), coef.vals, rep(outName[i], length(coef.vals)))
  
  output.values <- rbind(output.values, to.store)
}
## Now write the output
output.values2 <- output.values
rownames(output.values) <- NULL
output.values <- cbind(output.values, rep("RidgeCoef", dim(output.values)[1]))
write.csv(output.values, "ridgeImpMale.csv", quote=F, row.names=F)

## Now produce the values for the scatter plots
## First create an ROI vairbale w/o the modality nonsense
scat.vals <- as.data.frame(output.values2)
scat.vals$ROI <- rownames(scat.vals)

vals_to_rm <- c("mprage_jlf_vol_", "pcasl_jlf_cbf_", "mprage_jlf_gmd_", "dti_jlf_tr_", "rest_jlf_alff_", "rest_jlf_reho_")
for(i in vals_to_rm){
  scat.vals$ROI <- gsub(x=scat.vals$ROI, pattern=i, replacement='')
}
scat.vals$ROI <- gsub(x=scat.vals$ROI, pattern='.1', replacement='')
scat.vals <- reshape(data=scat.vals, direction="wide", idvar="ROI", timevar='V3', v.names='coef.vals')
#scat.vals <- reshape(data=scat.vals, direction="wide", idvar="ROI", timevar='V3', v.names='V2')
scat.vals[,3:dim(scat.vals)[2]] <- apply(scat.vals[,3:dim(scat.vals)[2]], 2, function(x) as.numeric(as.character(x)))
scat.vals$sumAbs <- rowSums(abs(scat.vals[, c(3:7, 9:10)]), na.rm=T)
scat.vals$sumAbs <- apply(scat.vals[,c(3:7, 9:10)], 1, function(x) mean(abs(x), na.rm=T))
scat.vals.targ <- scat.vals

## Now do it for females
output.values <- NULL
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==2),]
  tmpDatX <- scale(as.matrix(tmpDat[,grep(grepValue[i], names(tmpDat))]))
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  ## Now train a ridge reg model
  fit.cv <- cv.glmnet(x=tmpDatX, y=tmpDatY, alpha=0,nfolds=10)
  lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
  print(lambdaVal)
  modelFit <- glmnet(x=tmpDatX, y=tmpDatY, alpha=0, lambda=lambdaVal)
  coef.vals <- coef(modelFit)[-1,]
  to.store <- cbind(rownames(cbind(coef.vals, rep(outName[i], length(coef.vals)))), coef.vals, rep(outName[i], length(coef.vals)))
  output.values <- rbind(output.values, to.store)
}
## Now write the output
output.values2 <- output.values
rownames(output.values) <- NULL
output.values <- cbind(output.values, rep("Ridge", dim(output.values)[1]))
write.csv(output.values, "ridgeImpFemale.csv", quote=F, row.names=F)

## Now produce the values for the scatter plots
## First create an ROI vairbale w/o the modality nonsense
scat.vals <- as.data.frame(output.values2)
scat.vals$ROI <- rownames(scat.vals)

vals_to_rm <- c("mprage_jlf_vol_", "pcasl_jlf_cbf_", "mprage_jlf_gmd_", "dti_jlf_tr_", "rest_jlf_alff_", "rest_jlf_reho_")
for(i in vals_to_rm){
  scat.vals$ROI <- gsub(x=scat.vals$ROI, pattern=i, replacement='')
}
scat.vals$ROI <- gsub(x=scat.vals$ROI, pattern='.1', replacement='')
scat.vals <- reshape(data=scat.vals, direction="wide", idvar="ROI", timevar='V3', v.names='coef.vals')
#scat.vals <- reshape(data=scat.vals, direction="wide", idvar="ROI", timevar='V3', v.names='V2')
scat.vals[,3:dim(scat.vals)[2]] <- apply(scat.vals[,3:dim(scat.vals)[2]], 2, function(x) as.numeric(as.character(x)))
scat.vals$sumAbs <- apply(scat.vals[, c(3:7, 9:10)], 1, function(x) mean(abs(x), na.rm=T))

# Now plot these vals
mergeTarg <- merge(scat.vals.targ, scat.vals, by='ROI', suffixes=c('.Male','.Female'))
mergeTarg <- mergeTarg[-grep('dti_dtitk_jlf_fa', mergeTarg$ROI),]
#mergeTarg <- mergeTarg[-grep('Mean', mergeTarg$ROI),]
mergeTarg$lobe <- NULL
for(q in 1:length(mergeTarg$ROI)){mergeTarg$lobe[[q]] <- findLobe(mergeTarg$ROI[q])}
mergeTarg$lobe <- factor(mergeTarg$lobe)


## Now create an abs sum of all of these metrics
## Now plot these values
corVal <- paste("r = ", round(cor(mergeTarg$sumAbs.Female, mergeTarg$sumAbs.Male), digits=2))
plotOut <-  ggplot(mergeTarg, aes(y=sumAbs.Female, x=sumAbs.Male)) + 
  geom_point(aes(fill=lobe)) + 
  geom_smooth(method=lm) + 
  geom_label_repel(aes(label=ROI,size=3.5,color=lobe),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
  xlab("Male z-score difference") + 
  ylab("Female z-score difference") + 
  geom_hline(yintercept = 0 , linetype=3) + 
  geom_vline(xintercept = 0 , linetype=3) + 
  theme(legend.position="none",text = element_text(size = 16, lineheight = 1, face='bold')) + 
  geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal)) + geom_abline(intercept=0, slope=1)

pdf('absSumEFRidgeMEAN.pdf', width=12, height=12)
plotOut
dev.off()
