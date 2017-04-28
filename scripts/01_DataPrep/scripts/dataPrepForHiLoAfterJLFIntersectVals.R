# AFGR September 1st 2016

# This script is going to be used to perform all of the data prep for the Ruben Hi-Lo paper
# This will involve several tasks which are:
# 1.) Average Left and Right ROI's
# 2.) Regress out age-age^2-age^3 & volume data
# 3.) Regress out all other ROI's wihtin a modality within a ROI 

# I am going to try to build functions for each of these tasks because how many times
# have I had to re-do these freaking tasks...... toooo many.

# Load library(s)
source('/home/adrose/dataPrepForHiLoPaper/scripts/functions.R')

# Now load the data
data.values <- read.csv('/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv')
health.values <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/healthData/healthexclude_ltn.csv')
modal.scores <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/cogData2017/20170308/CNB_Factor_Scores_GO1-GO2-GO3.csv',header=TRUE)
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf-Impute.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCt.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfReho.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfAlff.csv')
md.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfTR.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuFATracts.csv')

# Find the subjects that had their cnb within one year of their imaging session
scan.Value <- data.values$ageAtGo1Scan
cnb.values <- data.values$ageAtGo1Cnb
diff.values <- scan.Value - cnb.values
acceptable.subjs <- which(diff.values <=12)
bblid.index <- data.values$bblid[acceptable.subjs]
scanid.index <- data.values$scanid[acceptable.subjs]

# Now limit this bblid index to those that also pass the health exclusions 
bblid.index <- bblid.index[bblid.index %in% health.values$bblid[which(health.values$incidentalExclude==0)]]
bblid.index <- bblid.index[bblid.index %in% volume.data$bblid[which(volume.data$t1Exclude==0)]]

# Now now limit to these subjects who meet the time criteria 
volume.data <- volume.data[volume.data$bblid %in% bblid.index,]
cbf.data <- cbf.data[cbf.data$bblid %in% bblid.index,]
gmd.data <- gmd.data[gmd.data$bblid %in% bblid.index,]
ct.data <- ct.data[ct.data$bblid %in% bblid.index,]
reho.data <- reho.data[reho.data$bblid %in% bblid.index,]
alff.data <- alff.data[alff.data$bblid %in% bblid.index,]
md.data <- md.data[md.data$bblid %in% bblid.index,]
fa.data <- fa.data[fa.data$bblid %in% bblid.index,]

# Now apply any imaging exclusionary criteria 
volume.data <- volume.data[which(volume.data$t1Exclude!=1),]
cbf.data <- cbf.data[which(cbf.data$pcaslExclude!=1),]
gmd.data <- gmd.data[which(gmd.data$t1Exclude!=1),]
ct.data <- ct.data[which(ct.data$t1Exclude!=1),]
reho.data <- reho.data[which(reho.data$restExclude!=1),]
alff.data <- alff.data[which(alff.data$restExclude!=1),]
md.data <- md.data[which(md.data$include_wT1ex_64==1),]
fa.data <- fa.data[which(fa.data$include_wT1ex_64==1),]

# Attach modality scores
modal.scores <- modal.scores[which(modal.scores$timepoint==1),]
volume.data <- merge(modal.scores, volume.data, by='bblid')
cbf.data <- merge(modal.scores, cbf.data, by='bblid')
gmd.data <- merge(modal.scores, gmd.data, by='bblid')
ct.data <- merge(modal.scores, ct.data, by='bblid')
reho.data <- merge(modal.scores, reho.data, by='bblid')
alff.data <- merge(modal.scores, alff.data, by='bblid')
md.data <- merge(modal.scores, md.data, by='bblid')
fa.data <- merge(modal.scores, fa.data, by='bblid')

# Attach sex and age
volume.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(volume.data$bblid, data.values$bblid)]
volume.data$sex <- data.values$sex[match(volume.data$bblid, data.values$bblid)]
cbf.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(cbf.data$bblid, data.values$bblid)]
cbf.data$sex <- data.values$sex[match(cbf.data$bblid, data.values$bblid)]
gmd.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(gmd.data$bblid, data.values$bblid)]
gmd.data$sex <- data.values$sex[match(gmd.data$bblid, data.values$bblid)]
ct.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(ct.data$bblid, data.values$bblid)]
ct.data$sex <- data.values$sex[match(ct.data$bblid, data.values$bblid)]
reho.data$sex <- data.values$sex[match(reho.data$bblid, data.values$bblid)]
reho.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(reho.data$bblid, data.values$bblid)]
alff.data$sex <- data.values$sex[match(alff.data$bblid, data.values$bblid)]
alff.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(alff.data$bblid, data.values$bblid)]
md.data$sex <- data.values$sex[match(md.data$bblid, data.values$bblid)]
md.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(md.data$bblid, data.values$bblid)]
fa.data$sex <- data.values$sex[match(fa.data$bblid, data.values$bblid)]
fa.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(fa.data$bblid, data.values$bblid)]

# This is where steps will divereg, I will have to prepare a varitey of data sets which include:
#	1.) Mean L and R - no regressing
#	2.) Mean L and R with Age regression and Volume regressing
#	3.) Mean L and R with Age and Modality Regressing and Volume regressing 
# in order to facilitate this process I am going to save the raw data frame as I have them out now first and perform
# each of these steps in its own little secluded section

## RM the cerbellar WM as this kills all variqnce when modality regressing
#volume.data <- volume.data[,-grep('Cerebral_White_Matter', names(volume.data))]
#cbf.data <- cbf.data[,-grep('Cerebral_White_Matter', names(cbf.data))]


## Prepare the raw data
volume.data.raw <- volume.data
cbf.data.raw <- cbf.data
gmd.data.raw <- gmd.data
ct.data.raw <- ct.data
reho.data.raw <- reho.data
alff.data.raw <- alff.data

# Now limit cbf data only to complete cases
cbf.data.raw <- cbf.data.raw[complete.cases(cbf.data),]

## 1.) Mean L and R and regress quality variable
volume.data <- averageLeftAndRight(volume.data.raw)
volume.data <- regressOutQuality(dataFrame=volume.data, modalityName='mprage_jlf_vol', qualityName='averageManualRating')
cbf.data <- averageLeftAndRight(cbf.data.raw)
cbf.data <- regressOutQuality(dataFrame=cbf.data, modalityName='pcasl_jlf_cbf', qualityName='pcaslRelMeanRMSMotion')
gmd.data <- averageLeftAndRight(gmd.data.raw)
gmd.data <- regressOutQuality(dataFrame=gmd.data, modalityName='mprage_jlf_gmd', qualityName='averageManualRating')
ct.data <- averageLeftAndRight(ct.data.raw)
ct.data <- regressOutQuality(dataFrame=ct.data, modalityName='mprage_jlf_ct', qualityName='averageManualRating')
reho.data <- averageLeftAndRight(reho.data.raw)
reho.data <- regressOutQuality(dataFrame=reho.data, modalityName='rest_jlf_reho', qualityName='restRelMeanRMSMotion')
alff.data <- averageLeftAndRight(alff.data.raw)
alff.data <- regressOutQuality(dataFrame=alff.data, modalityName='rest_jlf_alff', qualityName='restRelMeanRMSMotion')

# Now write the output for section 1
write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv', quote=F, row.names=F)
write.csv(gmd.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv', quote=F, row.names=F)
write.csv(ct.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv', quote=F, row.names=F)
write.csv(reho.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv', quote=F, row.names=F)
write.csv(alff.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv', quote=F, row.names=F)
#write.csv(md.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLR/mdData.csv', quote=F, row.names=F)

## 2.) Age regress and then Mean L and R
## It is important to note that we do not volume regress the CBF or the Vol data sets!!

# First we will regress out age and volume from the data sets
volume.data <- volume.data.raw
cbf.data <- cbf.data.raw
cbf.data[,grep('pcasl_jlf_cbf',names(cbf.data.raw))] <- apply(cbf.data.raw[,grep('pcasl_jlf_cbf',names(cbf.data.raw))], 2, function(x) regressOutAge(x, cbf.data$ageAtGo1Scan, cbf.data$pcaslRelMeanRMSMotion))
gmd.data <- gmd.data.raw
#gmd.data <- regressOutVolumeAndAge(volume.data, gmd.data, 'mprage_jlf_gmd')
gmd.data[,grep('mprage_jlf_gmd', names(gmd.data))] <- apply(gmd.data[,grep('mprage_jlf_gmd',names(gmd.data))], 2, function(x) regressOutAge(x, gmd.data$ageAtGo1Scan, gmd.data$averageManualRating))
ct.data <- ct.data.raw
#ct.data <- regressOutVolumeAndAge(volume.data, ct.data, 'mprage_jlf_ct')
ct.data[,grep('mprage_jlf_ct', names(ct.data))] <- apply(ct.data[,grep('mprage_jlf_ct',names(ct.data))], 2, function(x) regressOutAge(x, ct.data$ageAtGo1Scan,ct.data$averageManualRating))
reho.data <- reho.data.raw
#reho.data <- regressOutVolumeAndAge(volume.data, reho.data, 'rest_jlf_reho')
reho.data[,grep('rest_jlf_reho', names(reho.data))] <- apply(reho.data[,grep('rest_jlf_reho', names(reho.data))], 2, function(x) regressOutAge(x, reho.data$ageAtGo1Scan,reho.data$restRelMeanRMSMotion))
alff.data <- alff.data.raw
#alff.data <- regressOutVolumeAndAge(volume.data, alff.data, 'rest_jlf_alff')
alff.data[,grep('rest_jlf_alff', names(alff.data))] <- apply(alff.data[,grep('rest_jlf_alff', names(alff.data))], 2, function(x) regressOutAge(x, alff.data$ageAtGo1Scan, alff.data$restRelMeanRMSMotion))
volume.data[,29:176] <- apply(volume.data[,29:176], 2, function(x) regressOutAge(x, volume.data$ageAtGo1Scan, volume.data$averageManualRating))

# Now average the left and right data sets
volume.data <- averageLeftAndRight(volume.data)
cbf.data <- averageLeftAndRight(cbf.data)
gmd.data <- averageLeftAndRight(gmd.data)
ct.data <- averageLeftAndRight(ct.data)
reho.data <- averageLeftAndRight(reho.data)
alff.data <- averageLeftAndRight(alff.data)

# Now write the outputs for section 2
write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv', quote=F, row.names=F)
write.csv(gmd.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv', quote=F, row.names=F)
write.csv(ct.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv', quote=F, row.names=F)
write.csv(reho.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv', quote=F, row.names=F)
write.csv(alff.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv', quote=F, row.names=F)

## 3.) Age regress, Mean L and R, and then regress within modality 
## Its going to be the same steps as above, just with the modality regression added on
# First we will regress out age and volume from the data sets
volume.data <- volume.data.raw
cbf.data <- cbf.data.raw
cbf.data[,grep('pcasl_jlf_cbf',names(cbf.data.raw))] <- apply(cbf.data.raw[,grep('pcasl_jlf_cbf',names(cbf.data.raw))], 2, function(x) regressOutAge(x, cbf.data$ageAtGo1Scan, cbf.data$pcaslRelMeanRMSMotion))
gmd.data <- gmd.data.raw
#gmd.data <- regressOutVolumeAndAge(volume.data, gmd.data, 'mprage_jlf_gmd')
gmd.data[,grep('mprage_jlf_gmd', names(gmd.data))] <- apply(gmd.data[,grep('mprage_jlf_gmd',names(gmd.data))], 2, function(x) regressOutAge(x, gmd.data$ageAtGo1Scan, gmd.data$averageManualRating))
ct.data <- ct.data.raw
#ct.data <- regressOutVolumeAndAge(volume.data, ct.data, 'mprage_jlf_ct')
ct.data[,grep('mprage_jlf_ct', names(ct.data))] <- apply(ct.data[,grep('mprage_jlf_ct',names(ct.data))], 2, function(x) regressOutAge(x, ct.data$ageAtGo1Scan,ct.data$averageManualRating))
reho.data <- reho.data.raw
#reho.data <- regressOutVolumeAndAge(volume.data, reho.data, 'rest_jlf_reho')
reho.data[,grep('rest_jlf_reho', names(reho.data))] <- apply(reho.data[,grep('rest_jlf_reho', names(reho.data))], 2, function(x) regressOutAge(x, reho.data$ageAtGo1Scan,reho.data$restRelMeanRMSMotion))
alff.data <- alff.data.raw
#alff.data <- regressOutVolumeAndAge(volume.data, alff.data, 'rest_jlf_alff')
alff.data[,grep('rest_jlf_alff', names(alff.data))] <- apply(alff.data[,grep('rest_jlf_alff', names(alff.data))], 2, function(x) regressOutAge(x, alff.data$ageAtGo1Scan, alff.data$restRelMeanRMSMotion))
volume.data[,29:176] <- apply(volume.data[,29:176], 2, function(x) regressOutAge(x, volume.data$ageAtGo1Scan, volume.data$averageManualRating))

# Now average the left and right data sets
volume.data <- averageLeftAndRight(volume.data)
cbf.data <- averageLeftAndRight(cbf.data)
gmd.data <- averageLeftAndRight(gmd.data)
ct.data <- averageLeftAndRight(ct.data)
reho.data <- averageLeftAndRight(reho.data)
alff.data <- averageLeftAndRight(alff.data)

# Now perform the modality regressing
volume.data <- volume.data[,-grep('Cerebral_White_Matter', names(volume.data))]
#volume.data <- volume.data[,-grep('WM', names(volume.data))]
volume.data <- regressWithinModality(volume.data, 'mprage_jlf_vol')
cbf.data <- cbf.data[,-grep('Cerebral_White_Matter', names(cbf.data))]
cbf.data <- cbf.data[,-grep('WM', names(cbf.data))]
cbf.data <- regressWithinModality(cbf.data, 'pcasl_jlf_cbf')
gmd.data <- regressWithinModality(gmd.data, 'mprage_jlf_gmd')
ct.data <- regressWithinModality(ct.data, 'mprage_jlf_ct')
reho.data <- regressWithinModality(reho.data, 'rest_jlf_reho')
alff.data <- regressWithinModality(alff.data, 'rest_jlf_alff')

# Now write the outputs for section 3
write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/cbfData.csv', quote=F, row.names=F)
write.csv(gmd.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/gmdData.csv', quote=F, row.names=F)
write.csv(ct.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/ctData.csv', quote=F, row.names=F)
write.csv(reho.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/rehoData.csv', quote=F, row.names=F)
write.csv(alff.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/alffData.csv', quote=F, row.names=F)


# Now do everything for all of the DTI modalities down here 
# STart with the tracts and labels
prefix <- c("jhu", "jhu")
modal <- c("AD", "FA", "RD", "TR")
suffix <- c("Labels", "Tracts")
basepath <- "/home/adrose/dataPrepForHiLoPaper/data/rawData/"
basepathout <- "/home/adrose/dataPrepForHiLoPaper/data/"
for (suf in suffix) {
  for (mod in modal) {
    for (pre in prefix) {
      filePath <- paste(basepath, "n1601_", pre, mod, suf,".csv", sep = "")
      print(filePath)
      tmp <- read.csv(filePath)
      tmp <- tmp[tmp$bblid %in% bblid.index, ]
      tmp <- tmp[which(tmp$include_wT1ex_64 == 1), ]
      tmp <- merge(modal.scores, tmp, by='bblid')
      tmp$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(tmp$bblid, data.values$bblid)]
      tmp$sex <- data.values$sex[match(tmp$bblid, data.values$bblid)]
      tmp$TBV <- volume.data$mprage_antsCT_vol_TBV[match(tmp$bblid,volume.data$bblid)]
      avgLR <- paste(basepathout, "meanLR/", pre, mod,suf, "Data.csv", sep = "")
      avgLRVAR <- paste(basepathout, "meanLRVolandAgeReg/", pre, mod, suf, "Data.csv", sep = "")
      avgLRVAMR <- paste(basepathout, "meanLRVolandAgeRegModalReg/",pre, mod, suf, "Data.csv", sep = "")
      tmpLR <- averageLeftAndRight1(tmp)
      colnames(tmpLR) <- gsub(x=names(averageLeftAndRight1(tmp)), pattern='^_', replacement=paste('dti_dtitk_jhu', tolower(suf), '_rd_', sep=''))
      colnames(tmpLR) <- gsub(x=colnames(tmpLR), pattern=paste('^lic', sep=''), replacement=paste('dti_dtitk_jhu', tolower(suf), '_rd_', 'rlic', sep=''))
      write.csv(tmpLR, file=avgLR, quote=F, row.names=F)
      colsOfInterest <- grep("dti_dtitk_", names(tmp))
      tmpVR <- tmp
      print(dim(tmpVR))
      for(i in colsOfInterest){
        #tmpVR[,i] <- regressOutTBV(tmp[,i], tmp$TBV)
	tmpVR[,i] <- tmp[,i]
      }
      print(dim(tmpVR))
      tmpVRLR <- averageLeftAndRight1(tmpVR)
      colnames(tmpVRLR) <- gsub(x=colnames(tmpVRLR), pattern='^_', replacement=paste('dti_dtitk_jhu', tolower(suf), '_rd_', sep=''))
      colnames(tmpVRLR) <- gsub(x=colnames(tmpVRLR), pattern=paste('^lic', sep=''), replacement=paste('dti_dtitk_jhu', tolower(suf), '_rd_', 'rlic', sep=''))
      colnames(tmpVRLR) <- gsub(x=colnames(tmpVRLR), pattern=paste(tolower(mod), 'lic', sep=''), replacement=paste(tolower(mod), '_rlic', sep=''))
      write.csv(tmpVRLR, file=avgLRVAR, quote=F, row.names=F)
      tmpVR <- tmp
      print(dim(tmpVR))
      for(i in colsOfInterest){
        tmpVR[,i] <- regressOutTBV(tmp[,i], tmp$TBV)
      }
      print(dim(tmpVR))
      tmpVRLR <- averageLeftAndRight1(tmpVR)
      colnames(tmpVRLR) <- gsub(x=names(averageLeftAndRight1(tmp)), pattern='^_', replacement=paste('dti_dtitk_jhu', tolower(suf), '_rd_', sep=''))
      colnames(tmpVRLR) <- gsub(x=colnames(tmpLR), pattern=paste('^lic', sep=''), replacement=paste('dti_dtitk_jhu', tolower(suf), '_rd_', 'rlic', sep=''))
      colnames(tmpVRLR) <- gsub(x=colnames(tmpVRLR), pattern=paste(tolower(mod), 'lic', sep=''), replacement=paste(tolower(mod), '_rlic', sep=''))
      tmpVRLRMR <- regressWithinModality(tmpVRLR, 'dti_dtitk_jhu')
      write.csv(tmpVRLRMR, file=avgLRVAMR, quote=F, row.names=F)
    }
  }
}
## Now do the GM DTI values
# Now do everything for all of the DTI modalities down here 
prefix <- c("jlf")
modal <- c("AD", "FA", "RD", "TR")
suffix <- c("")
basepath <- "/home/adrose/dataPrepForHiLoPaper/data/rawData/"
basepathout <- "/home/adrose/dataPrepForHiLoPaper/data/"
for (suf in suffix) {
  for (mod in modal) {
    for (pre in prefix) {
      filePath <- paste(basepath, "n1601_", pre, mod, suf,".csv", sep = "")
      print(filePath)
      tmp <- read.csv(filePath)
      tmp <- tmp[tmp$bblid %in% bblid.index, ]
      tmp <- tmp[which(tmp$include_wT1ex_64 == 1), ]
      tmp <- merge(modal.scores, tmp, by='bblid')
      tmp$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(tmp$bblid, data.values$bblid)]
      tmp$sex <- data.values$sex[match(tmp$bblid, data.values$bblid)]
      tmp$TBV <- volume.data$mprage_antsCT_vol_TBV[match(tmp$bblid,volume.data$bblid)]
      avgLR <- paste(basepathout, "meanLR/", pre, mod,"Data.csv", sep = "")
      avgLRVAR <- paste(basepathout, "meanLRVolandAgeReg/", pre, mod,"Data.csv", sep = "")
      avgLRVAMR <- paste(basepathout, "meanLRVolandAgeRegModalReg/",pre, mod,"Data.csv", sep = "")
      tmpLR <- averageLeftAndRight(tmp)
      write.csv(tmpLR, file=avgLR, quote=F, row.names=F)
      colsOfInterest <- grep("dti_dtitk_", names(tmp))
      tmpVR <- tmp
      print(dim(tmpVR))
      for(i in colsOfInterest){
        #tmpVR[,i] <- regressOutTBV(tmp[,i], tmp$TBV)
	tmpVR[,i] <- tmp[,i]
      }
      print(dim(tmpVR))
      tmpVRLR <- averageLeftAndRight(tmpVR)
      write.csv(tmpVRLR, file=avgLRVAR, quote=F, row.names=F)
      tmpVR <- tmp
      print(dim(tmpVR))
      for(i in colsOfInterest){
        tmpVR[,i] <- regressOutTBV(tmp[,i], tmp$TBV)
      }
      print(dim(tmpVR))
      tmpVRLR <- averageLeftAndRight(tmpVR)
      tmpVRLRMR <- regressWithinModality(tmpVRLR, 'dti_jlf')      
      write.csv(tmpVRLRMR, file=avgLRVAMR, quote=F, row.names=F)
    }
  }
}
