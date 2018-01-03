# AFGR September 1st 2016

# This script is going to be used to perform all of the data prep for the Ruben Hi-Lo paper
# This will involve several tasks which are:
# 1.) Average Left and Right ROI's
# 2.) Regress out age-age^2-age^3 & volume data
# 3.) Regress out all other ROI's wihtin a modality within a ROI 

# I am going to try to build functions for each of these tasks because how many times
# have I had to re-do these freaking tasks...... toooo many.

# Load library(s)
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('psych')

# Now load the data
data.values <- read.csv('/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv')
health.values <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/healthData/healthexclude_ltn.csv')
modal.scores <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/cogData2017/20170308/CNB_Factor_Scores_GO1-GO2-GO3.csv',header=TRUE)
modal.scores <- modal.scores[which(modal.scores$timepoint==1),]
race.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_raceVals.csv')
ses.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n9498_go1_environment_factor_scores_tymoore_20150909.csv')
modal.scores <- merge(modal.scores, race.vals, by='bblid')
modal.scores <- merge(modal.scores, ses.vals, by='bblid')
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
volume.data <- volume.data[,-grep('Cerebral_White_Matter', names(volume.data))]
volume.data <- volume.data[,-grep("4th_Ventricle", names(volume.data))]
volume.data <- merge(modal.scores, volume.data, by='bblid')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf-Impute.csv')
cbf.data <- cbf.data[,-grep('Cerebral_White_Matter', names(cbf.data))]
cbf.data <- cbf.data[complete.cases(cbf.data),]
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCt.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCc.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfReho.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfAlff.csv')
md.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfTR.csv')
md.data <- md.data[complete.cases(md.data[,grep('dti_jlf_tr', names(md.data))]),]
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfFA.csv')
fa.data <- fa.data[complete.cases(fa.data[,grep('dti_jlf_fa', names(fa.data))]),]
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfAD.csv')
ad.data <- ad.data[complete.cases(ad.data[,grep('dti_jlf_ad', names(ad.data))]),]
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfRD.csv')
rd.data <- rd.data[complete.cases(rd.data[,grep('dti_jlf_rd', names(rd.data))]),]
fa.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuFALabels.csv')

# Find the subjects that had their cnb within one year of their imaging session
scan.Value <- data.values$ageAtGo1Scan
cnb.values <- data.values$ageAtGo1Cnb
diff.values <- scan.Value - cnb.values
acceptable.subjs <- which(diff.values <=12)
bblid.index <- data.values$bblid[acceptable.subjs]
scanid.index <- data.values$scanid[acceptable.subjs]

# Now limit this bblid index to those that also pass the health exclusions 
bblid.index <- bblid.index[bblid.index %in% health.values$bblid[which(health.values$incidentalExclude==0)]]
bblid.index.fa <- bblid.index
bblid.index <- bblid.index[bblid.index %in% volume.data$bblid[which(volume.data$t1Exclude==0)]]

# Now create a for loop to do everything for our GM values
dataVals <- c('cbf.data', 'gmd.data', 'ct.data', 'reho.data', 'alff.data', 'md.data', 'cc.data', 'fa.data', 'ad.data', 'rd.data', 'fa.label')
outNames <- c('cbfData.csv', 'gmdData.csv', 'ctData.csv', 'rehoData.csv', 'alffData.csv', 'jlfTRData.csv', 'ccData.csv', 'jlfFAData.csv', 'jlfADData.csv', 'jlfRDData.csv', 'jhuFALabel.csv')
modalNames <- c('pcasl_jlf_cbf', 'mprage_jlf_gmd', 'mprage_jlf_ct', 'rest_jlf_reho', 'rest_jlf_alff', 'dti_jlf_tr', 'mprage_jlf_cortcon', 'dti_jlf_fa', 'dti_jlf_ad', 'dti_jlf_rd', 'dti_dtitk_jhulabel')
excludeVals <- c('pcaslExclude', 't1Exclude', 't1Exclude', 'restExclude', 'restExclude', 'dti64Exclude', 't1Exclude','dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude')
outputMeanLR <- "/home/adrose/dataPrepForHiLoPaper/data/meanLR/"
outputAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageReg/"
outputMeanLRAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/"
outputMeanLRAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/"

# Now I am going to create a for loop which will output all of the required CSV's 
for(i in 1:length(dataVals)){
  # First load the data set, and create all of our output names
  tmpData <- get(dataVals[i])
  outMean <- paste(outputMeanLR, outNames[i], sep='')
  outAgeNM <- paste(outputAgeReg, outNames[i], sep='')
  outAge <- paste(outputMeanLRAgeReg, outNames[i], sep='')
  outMod <- paste(outputMeanLRAgeRegModReg, outNames[i], sep='')

  # Now apply our immediete restrictions
  if(i < 11){
    tmpData <- tmpData[tmpData$bblid %in% bblid.index,]
  }
  if(i > 10){
    tmpData <- tmpData[tmpData$bblid %in% bblid.index.fa,]
  }
  tmpData <- tmpData[which(tmpData[excludeVals[i]]!=1), ]
  tmpData <- merge(modal.scores, tmpData, by='bblid')

  # Now attach our demographic data
  tmpData$sex <- data.values$sex[match(tmpData$bblid, data.values$bblid)]
  tmpData$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(tmpData$bblid, data.values$bblid)]

  # produce our avgLR
  if(i < 11){
    tmpLR <- averageLeftAndRight(tmpData)
  }  
  if(i > 10){
    tmpLR <- averageLeftAndRight1(tmpData)
  }

  # Now produce our ageReg vals 
  tmpAR <- tmpData
  tmpAR[,grep(modalNames[i], names(tmpAR))] <- apply(tmpAR[,grep(modalNames[i], names(tmpAR))], 2, function(x) regressOutAgeNoQA(x, tmpAR$ageAtGo1Scan, tmpAR$envSES))
  write.csv(tmpAR, outAgeNM, quote=F, row.names=F)
  if(i < 11){
    tmpAR <- averageLeftAndRight(tmpAR)
  }  
  if(i > 10){
    tmpAR <- averageLeftAndRight1(tmpAR)
  }

  # Now for modality regression make sure we don't inuclude any global summary metrics
  colsToRM <- NULL
  colsToRM <- grep('_jlf_ICV', names(tmpAR))
  colsToRM <- append(colsToRM, grep('_Mean', names(tmpAR)))
  if(!identical(integer(0), colsToRM)){
    tmpMR <- tmpAR[, -colsToRM]
  } else if(identical(integer(0), colsToRM)){
    tmpMR <- tmpAR
  }

  # Now produce a modality regressed data frame
  tmpMR <- regressWithinModality(tmpMR, modalNames[i])

  # Now write the csvs
  write.csv(tmpLR, outMean, quote=F, row.names=F)
  write.csv(tmpAR, outAge, quote=F, row.names=F)
  write.csv(tmpMR, outMod, quote=F, row.names=F)
}

# Now produce the volume values
volIndex <- max(grep('mprage_jlf_vol_', names(volume.data)))
volume.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(volume.data$bblid, data.values$bblid)]
volume.data$sex <- data.values$sex[match(volume.data$bblid, data.values$bblid)]
volume.data <- volume.data[volume.data$bblid %in% bblid.index,]
volume.data2 <- volume.data
volAVG <- averageLeftAndRight(volume.data)
volume.data2[,33:volIndex] <- apply(volume.data2[,33:volIndex], 2, function(x) regressOutAgeNoQA(x, volume.data2$ageAtGo1Scan, volume.data2$envSES))
write.csv(volume.data2, "/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv", quote=F, row.names=F)
volume.data.2 <- averageLeftAndRight(volume.data2)
volume.data2 <- averageLeftAndRight(volume.data2)
volume.data.2 <- volume.data.2[,-c(40, 33, 34, 35)]
volume.data.MR <- regressWithinModality(volume.data.2, 'mprage_jlf_vol')
write.csv(volAVG, "/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv", quote=F, row.names=F)
write.csv(volume.data2, "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv", quote=F, row.names=F)
write.csv(volume.data.MR, "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv", quote=F, row.names=F)


q()
# Now do the same thing but add QA regressing 
outputMeanLR <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRQA/"
outputMeanLRAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/"
outputMeanLRAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalRegQA/"
qaVals <- c('pcaslRelMeanRMSMotion', 'averageManualRating', 'averageManualRating', 'restRelMeanRMSMotion', 'restRelMeanRMSMotion', 'tsnr_64', 'averageManualRating')
# Now run em all thorugh the loop
for(i in 1:length(dataVals)){
  # First load the data set, and create all of our output names
  tmpData <- get(dataVals[i])
  outMean <- paste(outputMeanLR, outNames[i], sep='')
  outAge <- paste(outputMeanLRAgeReg, outNames[i], sep='')
  outMod <- paste(outputMeanLRAgeRegModReg, outNames[i], sep='')

  # Now apply our immediete restrictions
  tmpData <- tmpData[tmpData$bblid %in% bblid.index,]
  tmpData <- tmpData[which(tmpData[excludeVals[i]]!=1), ]
  tmpData <- merge(modal.scores, tmpData, by='bblid')

  # Now attach our demographic data
  tmpData$sex <- data.values$sex[match(tmpData$bblid, data.values$bblid)]
  tmpData$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(tmpData$bblid, data.values$bblid)]

  # produce our avgLR
  tmpLR <- averageLeftAndRight(tmpData)
  tmpLR <- regressOutQuality(dataFrame=tmpLR, modalityName=modalNames[i], qualityName=qaVals[i])
  
  # Now produce our ageReg vals 
  tmpAR <- tmpData
  tmpAR[,grep(modalNames[i], names(tmpAR))] <- apply(tmpAR[,grep(modalNames[i], names(tmpAR))], 2, function(x) regressOutAge(x, tmpAR$ageAtGo1Scan, unlist(tmpAR[qaVals[i]]), tmpAR$envSES))
  tmpAR <- averageLeftAndRight(tmpAR)  

  # Now produce a modality regressed data frame
  tmpMR <- regressWithinModality(tmpAR, modalNames[i])

  # Now write the csvs
  write.csv(tmpLR, outMean, quote=F, row.names=F)
  write.csv(tmpAR, outAge, quote=F, row.names=F)
  write.csv(tmpMR, outMod, quote=F, row.names=F)
}

# Now produce the volume values
volume.data3 <- averageLeftAndRight(volume.data)
volume.data3 <- regressOutQuality(dataFrame=volume.data3, modalityName='mprage_jlf_vol', qualityName='averageManualRating')
volume.data4 <- volume.data
volume.data4[,33:volIndex] <- apply(volume.data4[,33:volIndex], 2, function(x) regressOutAge(x, volume.data4$ageAtGo1Scan, volume.data4$averageManualRating, volume.data4$envSES))
volume.data4 <- averageLeftAndRight(volume.data4)
volume.data.MR4 <- regressWithinModality(volume.data4, 'mprage_jlf_vol')
write.csv(volume.data3, '/home/adrose/dataPrepForHiLoPaper/data/meanLRQA/volumeData.csv', quote=F, row.names=F)
write.csv(volume.data4, "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/volumeData.csv", quote=F, row.names=F)
write.csv(volume.data.MR4, "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalRegQA/volumeData.csv", quote=F, row.names=F)
