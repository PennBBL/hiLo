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
curve.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCurve.csv')
curve.data <- curve.data[,-c(14,15,27,30,31,32,33,42,43,44,48,49,160,161)]
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf-Impute.csv')
cbf.data <- cbf.data[,-grep('Cerebral_White_Matter', names(cbf.data))]
cbf.data <- cbf.data[complete.cases(cbf.data),]
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCt.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCc.csv')
sa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfSa.csv')
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
tr.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuTRLabels.csv')
fa.tracts <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuFATracts.csv')

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
dataVals <- c('volume.data', 'cbf.data', 'curve.data', 'ct.data', 'sa.data', 'reho.data', 'alff.data', 'md.data', 'cc.data', 'fa.data', 'ad.data', 'rd.data', 'fa.label', 'tr.label', 'fa.tracts', 'curve.data')
outNames <- c('volumeData.csv', 'cbfData.csv', 'curveData.csv', 'ctData.csv', 'saData.csv','rehoData.csv', 'alffData.csv', 'jlfTRData.csv', 'ccData.csv', 'jlfFAData.csv', 'jlfADData.csv', 'jlfRDData.csv', 'jhuFALabel.csv', 'jhuTRLabel.csv', 'jhuFATracts.csv', 'curveData.csv')
modalNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf', 'mprage_jlf_curl', 'mprage_jlf_ct', 'mprage_jlf_sa','rest_jlf_reho', 'rest_jlf_alff', 'dti_jlf_tr', 'mprage_jlf_cortcon', 'dti_jlf_fa', 'dti_jlf_ad', 'dti_jlf_rd', 'dti_dtitk_jhulabel','dti_dtitk_jhulabel', 'dti_dtitk_jhutract_fa', 'mprage_jlf_curve_')
excludeVals <- c('t1Exclude', 'pcaslExclude', 't1Exclude', 't1Exclude', 't1Exclude','restExclude', 'restExclude', 'dti64Exclude', 't1Exclude','dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude','t1Exclude')
outputMeanLR <- "/home/adrose/dataPrepForHiLoPaper/data/meanLR/"
outputAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageReg/"
outputMeanLRAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/"
outputMeanLRAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/"
outputAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/"
outputNoReg <- "/home/adrose/dataPrepForHiLoPaper/data/noReg/"

# Now I am going to create a for loop which will output all of the required CSV's 
for(i in 1:length(dataVals)){
  # First load the data set, and create all of our output names
  tmpData <- get(dataVals[i])
  tmpData <- tmpData[complete.cases(tmpData[,grep(modalNames[i], names(tmpData))]),]
  outMean <- paste(outputMeanLR, outNames[i], sep='')
  outAgeNM <- paste(outputAgeReg, outNames[i], sep='')
  outAgeNMMod <- paste(outputAgeRegModReg, outNames[i], sep='')
  outAge <- paste(outputMeanLRAgeReg, outNames[i], sep='')
  outMod <- paste(outputMeanLRAgeRegModReg, outNames[i], sep='')
  outNR <- paste(outputNoReg, outNames[i], sep='')

  # Now apply our immediete restrictions
  if(i < 13){
    tmpData <- tmpData[tmpData$bblid %in% bblid.index,]
  }
  if(i > 12){
    tmpData <- tmpData[tmpData$bblid %in% bblid.index.fa,]
  }
  tmpData <- tmpData[which(tmpData[excludeVals[i]]!=1), ]
  tmpData <- merge(modal.scores, tmpData)

  # Now attach our demographic data
  tmpData$sex <- data.values$sex[match(tmpData$bblid, data.values$bblid)]
  tmpData$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(tmpData$bblid, data.values$bblid)]
  write.csv(tmpData, outNR, quote=F, row.names=F)

  # produce our avgLR
  if(i < 13){
    tmpLR <- averageLeftAndRight(tmpData)
  }  
  if(i > 12){
    tmpLR <- averageLeftAndRight1(tmpData)
  }

  # Now produce our ageReg vals 
  tmpAR <- tmpData
  tmpAR[,grep(modalNames[i], names(tmpAR))] <- apply(tmpAR[,grep(modalNames[i], names(tmpAR))], 2, function(x) regressOutAgeNoQA(x, tmpAR$ageAtGo1Scan, tmpAR$envSES))
  write.csv(tmpAR, outAgeNM, quote=F, row.names=F)

  # Now produce age and modal reg values
  colsToRM <- NULL
  colsToRM <- grep('ICV', names(tmpAR))
  colsToRM <- append(colsToRM, grep('Mean', names(tmpAR)))
  if(!identical(integer(0), colsToRM)){
    tmpMR <- tmpAR[, -colsToRM]
  } else if(identical(integer(0), colsToRM)){
    tmpMR <- tmpAR
  }
  tmpMR <- regressWithinModality(tmpMR, modalNames[i])
  write.csv(tmpMR, outAgeNMMod, quote=F, row.names=F)  

  # Now produce the meaned across hemisphere and modality regressed values  
  if(i < 13){
    tmpAR <- averageLeftAndRight(tmpAR)
  }  
  if(i > 12){
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

# Now do the same thing but add QA regressing 
outputMeanLR <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRQA/"
outputMeanLRAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/"
outputAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageRegQA/"
outputMeanLRAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalRegQA/"
outputAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageRegModalRegQA/"
qaVals <- c('averageManualRating','pcaslRelMeanRMSMotion', 'averageManualRating', 'averageManualRating', 'restRelMeanRMSMotion', 'restRelMeanRMSMotion', 'dti64Tsnr', 'averageManualRating',  'dti64Tsnr','dti64Tsnr','dti64Tsnr','dti64Tsnr','dti64Tsnr','dti64Tsnr')

# Now run em all thorugh the loop
for(i in 1:length(dataVals)){
  # First load the data set, and create all of our output names
  tmpData <- get(dataVals[i])
  tmpData <- tmpData[complete.cases(tmpData[,grep(modalNames[i], names(tmpData))]),]
  outMean <- paste(outputMeanLR, outNames[i], sep='')
  outAgeNM <- paste(outputAgeReg, outNames[i], sep='')
  outAgeNMMod <- paste(outputAgeRegModReg, outNames[i], sep='')
  outAge <- paste(outputMeanLRAgeReg, outNames[i], sep='')
  outMod <- paste(outputMeanLRAgeRegModReg, outNames[i], sep='')

  # Now apply our immediete restrictions
  if(i < 13){
    tmpData <- tmpData[tmpData$bblid %in% bblid.index,]
  }
  if(i > 12){
    tmpData <- tmpData[tmpData$bblid %in% bblid.index.fa,]
  }
  tmpData <- tmpData[which(tmpData[excludeVals[i]]!=1), ]
  tmpData <- merge(modal.scores, tmpData)

  # Now attach our demographic data
  tmpData$sex <- data.values$sex[match(tmpData$bblid, data.values$bblid)]
  tmpData$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(tmpData$bblid, data.values$bblid)]

  # produce our avgLR
  if(i < 13){
    tmpLR <- averageLeftAndRight(tmpData)
    tmpLR <- regressOutQuality(dataFrame=tmpLR, modalityName=modalNames[i], qualityName=qaVals[i])
  }  
  if(i > 12){
    tmpLR <- averageLeftAndRight1(tmpData)
    tmpLR <- regressOutQuality(dataFrame=tmpLR, modalityName=modalNames[i], qualityName=qaVals[i])
  }

  # Now produce our ageReg vals 
  tmpAR <- tmpData
  tmpAR[,grep(modalNames[i], names(tmpAR))] <- apply(tmpAR[,grep(modalNames[i], names(tmpAR))], 2, function(x) regressOutAgeNoQA(x, tmpAR$ageAtGo1Scan, tmpAR$envSES))
  write.csv(tmpAR, outAgeNM, quote=F, row.names=F)

  # Now produce age and modal reg values
  colsToRM <- NULL
  colsToRM <- grep('_vol_ICV', names(tmpAR))
  colsToRM <- append(colsToRM, grep('_Mean', names(tmpAR)))
  if(!identical(integer(0), colsToRM)){
    tmpMR <- tmpAR[, -colsToRM]
  } else if(identical(integer(0), colsToRM)){
    tmpMR <- tmpAR
  }
  tmpMR <- regressWithinModality(tmpMR, modalNames[i])
  write.csv(tmpMR, outAgeNMMod, quote=F, row.names=F)

  if(i < 13){
    tmpAR <- averageLeftAndRight(tmpAR)
  }  
  if(i > 12){
    tmpAR <- averageLeftAndRight1(tmpAR)
  }

  # Now for modality regression make sure we don't inuclude any global summary metrics
  colsToRM <- NULL
  colsToRM <- grep('_vol_ICV', names(tmpAR))
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

## Now do the longitudinal data down here
## Start by loading the data
long.data.vals <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/allLongData.csv")
long.data.vals <- long.data.vals[,1:19]
demo.values <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n2416_demographics_20170310.csv")
modal.scores <- merge(long.data.vals, demo.values, all=T)
ses.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n9498_go1_environment_factor_scores_tymoore_20150909.csv')
modal.scores <- merge(modal.scores, ses.vals)
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_antsCtVol_jlfVol.csv')
volume.data <- volume.data[,-grep('Cerebral_White_Matter', names(volume.data))]
volume.data <- volume.data[,-grep("4th_Ventricle", names(volume.data))]
volume.data <- merge(modal.scores, volume.data)
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfCbf-Impute.csv')
cbf.data <- cbf.data[,-grep('Cerebral_White_Matter', names(cbf.data))]
cbf.data <- cbf.data[complete.cases(cbf.data),]
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfCt.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfCc.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfReho.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfAlff.csv')
md.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfTR.csv')
md.data <- md.data[complete.cases(md.data[,grep('dti_jlf_tr', names(md.data))]),]
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfFA.csv')
fa.data <- fa.data[complete.cases(fa.data[,grep('dti_jlf_fa', names(fa.data))]),]
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfAD.csv')
ad.data <- ad.data[complete.cases(ad.data[,grep('dti_jlf_ad', names(ad.data))]),]
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jlfRD.csv')
rd.data <- rd.data[complete.cases(rd.data[,grep('dti_jlf_rd', names(rd.data))]),]
fa.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jhuFALabels.csv')
tr.label <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_jhuTRLabels.csv')

# Now create a for loop to do everything for our GM values
dataVals <- c('volume.data', 'cbf.data', 'gmd.data', 'ct.data', 'reho.data', 'alff.data', 'md.data', 'cc.data', 'fa.data', 'ad.data', 'rd.data', 'fa.label', 'tr.label')
outNames <- c('volumeData.csv', 'cbfData.csv', 'gmdData.csv', 'ctData.csv', 'rehoData.csv', 'alffData.csv', 'jlfTRData.csv', 'ccData.csv', 'jlfFAData.csv', 'jlfADData.csv', 'jlfRDData.csv', 'jhuFALabel.csv', 'jhuTRLabel.csv', 'jhuFATracts.csv')
modalNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf', 'mprage_jlf_gmd', 'mprage_jlf_ct', 'rest_jlf_reho', 'rest_jlf_alff', 'dti64_jlf_tr', 'mprage_jlf_cortcon', 'dti64_jlf_fa', 'dti64_jlf_ad', 'dti64_jlf_rd', 'dti_dtitk_jhulabel','dti_dtitk_jhulabel', 'dti_dtitk_jhutract_fa')
excludeVals <- c('t1Exclude', 'pcaslExclude', 't1Exclude', 't1Exclude', 'restExclude', 'restExclude', 'dti64Exclude', 't1Exclude','dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude')
outputMeanLR <- "/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/"
outputAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageReg2416/"
outputMeanLRAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg2416/"
outputMeanLRAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg2416/"
outputAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg2416/"

# Now I am going to create a for loop which will output all of the required CSV's 
for(i in 1:length(dataVals)){
  # First load the data set, and create all of our output names
  tmpData <- get(dataVals[i])
  tmpData <- tmpData[complete.cases(tmpData[,grep(modalNames[i], names(tmpData))]),]
  outMean <- paste(outputMeanLR, outNames[i], sep='')
  outAgeNM <- paste(outputAgeReg, outNames[i], sep='')
  outAgeNMMod <- paste(outputAgeRegModReg, outNames[i], sep='')
  outAge <- paste(outputMeanLRAgeReg, outNames[i], sep='')
  outMod <- paste(outputMeanLRAgeRegModReg, outNames[i], sep='')

  # Combine our psychological data and modality data
  tmpData <- tmpData[which(tmpData[excludeVals[i]]!=1), ]
  tmpData <- merge(modal.scores, tmpData)

  # Now attach our demographic data
  tmpData$ageAtGo1Scan <- tmpData$scanageMonths

  # produce our avgLR
  if(i < 12){
    tmpLR <- averageLeftAndRight(tmpData)
  }  
  if(i > 11){
    tmpLR <- averageLeftAndRight1(tmpData)
  }

  # Now produce our ageReg vals 
  tmpAR <- tmpData
  tmpAR[,grep(modalNames[i], names(tmpAR))] <- apply(tmpAR[,grep(modalNames[i], names(tmpAR))], 2, function(x) regressOutAgeNoQA(x, tmpAR$ageAtGo1Scan, tmpAR$envSES))
  write.csv(tmpAR, outAgeNM, quote=F, row.names=F)

  # Now produce age and modal reg values
  colsToRM <- NULL
  colsToRM <- grep('ICV', names(tmpAR))
  colsToRM <- append(colsToRM, grep('Mean', names(tmpAR)))
  if(!identical(integer(0), colsToRM)){
    tmpMR <- tmpAR[, -colsToRM]
  } else if(identical(integer(0), colsToRM)){
    tmpMR <- tmpAR
  }
  tmpMR <- regressWithinModality(tmpMR, modalNames[i])
  write.csv(tmpMR, outAgeNMMod, quote=F, row.names=F)  

  # Now produce the meaned across hemisphere and modality regressed values  
  if(i < 12){
    tmpAR <- averageLeftAndRight(tmpAR)
  }  
  if(i > 11){
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
