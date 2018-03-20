# load all data
basePath <- '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_'
# Create a loop to load, and merge all data
allData <- read.csv(paste(basePath, 'antsCtVol_jlfVol.csv', sep=''))
allData <- allData[,-grep('Cerebral_White_Matter', names(allData))]
allData[which(allData$t1Exclude==1),grep('mprage_jlf_vol_', names(allData))] <- NA

suffixVals <- c('jlfCt.csv','jlfGMD.csv','jlfCc.csv','jlfCbf-Impute.csv','jlfTR.csv','jlfRD.csv','jlfAD.csv','jlfFA.csv','jlfAlff.csv','jlfReho.csv','jhuTRTracts.csv','jhuRDTracts.csv','jhuADTracts.csv','jhuFATracts.csv')
exclusionVals <- c('t1Exclude', 't1Exclude', 't1Exclude', 'pcaslExclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude','restExclude', 'restExclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude')
grepVals <- c('mprage_jlf_', 'mprage_jlf_', 'mprage_jlf', 'pcasl_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'rest_jlf', 'rest_jlf', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_')
binaryVal <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(z in 1:length(suffixVals)){
  # Now apply the exclusion
  i <- suffixVals[z] 
  e <- exclusionVals[z]
  bV <- binaryVal[z]
  g <- grepVals[z]
  tmp <- read.csv(paste(basePath, i, sep=''))
  print(paste(z,dim(tmp)))
  tmp[which(tmp[e]!=bV),grep(g, names(tmp))] <- NA
  if(z > 4){
    checkGrep <- grep('t1Exclude', names(tmp))
    if(length(checkGrep) > 0 ){
      tmp <- tmp[,-checkGrep]
    }
  }  
  allData <- merge(allData, tmp, by=intersect(names(allData), names(tmp)), all=T)
  print(dim(allData))
}

# Now create our summary metrics
# These will be weighted averages by volume across the entire brain
volData <- allData[,c(1,2,grep('mprage_jlf_vol_', names(allData)))]
ctVals <- allData[,c(1,2,grep('mprage_jlf_ct_', names(allData)))]
cbfVals <- allData[,c(1,2,grep('pcasl_jlf_cbf', names(allData)))]
cbfVals <- cbfVals[,-grep('_Cerebral_White_Matter', names(cbfVals))]
cbfWMVals <- cbfVals[,c(1,2,116:127)]
rehoVals <- allData[,c(1,2,grep('rest_jlf_reho_', names(allData)))]
alffVals <- allData[,c(1,2,grep('rest_jlf_alff_', names(allData)))]
trVals <- allData[,c(1,2,grep('dti_jlf_tr_', names(allData)))]
trVals <- trVals[,-grep('_MeanTR', names(trVals))]
trWMVals <- trVals[,c(1,2,7,121:132)]
faVals <- allData[,c(1,2,grep('dti_jlf_fa_', names(allData)))]
faWMVals <- faVals[,c(1,2,7,120:131)]
dataValue <- c('ctVals', 'cbfVals', 'rehoVals', 'alffVals', 'trVals', 'cbfWMVals', 'trWMVals','faWMVals')
nameRep <- c('mprage_jlf_ct_','pcasl_jlf_cbf_','rest_jlf_reho_','rest_jlf_alff_','dti_jlf_tr_','pcasl_jlf_cbf_','dti_jlf_tr_','dti_jlf_fa_')
output <- NULL
for(q in 1:length(dataValue)){
  tmpDat <- get(dataValue[q])
  volNames <- gsub(names(volData), pattern='mprage_jlf_vol_', replacement='')
  tmpNames <- gsub(names(tmpDat), pattern=nameRep[q], replacement='') 
  tmpVol <- volData[,volNames %in% tmpNames] 
  print(dim(tmpVol))
  print(dim(tmpDat))
  tmpOutVals <- NULL
  for(z in 1:1601){
    weightedVal <- weighted.mean(tmpDat[z,-c(1,2)],tmpVol[z,-c(1,2)], na.rm=T)
    tmpOutVals <- append(tmpOutVals, weightedVal)
  }
  output <- cbind(output, tmpOutVals)
}

## Now produce a TBV value
## This will include all of the parenchyma
## we will also produce a total GM and WM values here. 
## I am going to call the brain stem a WM ROI
TBVValsOut <- volData[,c(1,2,5:15, 17,18,23:141)]
TBVValsOut <- apply(TBVValsOut[,-c(1,2)], 1, sum)
GMValsOut <- volData[,c(1,2,5,6,10,11,12,13,17,18,23:129)]
GMValsOut <- apply(GMValsOut[,-c(1,2)], 1, sum)
WMValsOut <- volData[,c(1,2,9,14,15,130:141)]
WMValsOut <- apply(WMValsOut[,-c(1,2)], 1, sum)

# Now I need to do the same with just the GM regions
output <- cbind(output, allData$pcaslMeanGMValue,allData$dti_jlf_tr_MeanTR, TBVValsOut, GMValsOut, WMValsOut)
colnames(output) <- c('mprage_jlf_ct_MeanCT', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF', 'dti_jlf_tr_MeanWholeBrainTR', 'pcasl_jlf_cbf_MeanGMCBF', 'dti_jlf_tr_MeanGMTR','pcasl_jlf_cbf_MeanWMCBF','dti_jlf_tr_MeanWMTR','dti_jlf_fa_MeanWMFA','mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM')
allData <- cbind(allData, output)

# Now produce lobar values
# Start with just volume
lobarValues <- c(100, 101, 102, 103, 104, 105, 106, 107, 108,109, 104, 105, 108, 109, 100, 101, 104, 105, 104, 105, 122,123, 104, 105, 108, 109, 122, 123, 108, 109, 104, 105, 100, 101, 104, 105, 104, 105, 108, 109, 104, 105, 106, 107, 104, 105, 104, 105, 122, 123, 108, 109, 108, 109, 104, 105, 104, 105, 100, 101, 106, 107, 100, 101, 102, 103, 106, 107, 106, 107, 104, 105, 122, 123, 104, 105, 104, 105, 104, 105, 106, 107, 108, 109, 106, 107, 122, 123, 122, 123, 104, 105, 122, 123)
volVals <- 104:201
tmpVolVals <- allData[, volVals]
tmpOut <- NULL
for (v in names(table(lobarValues))){
sumVal <- apply(tmpVolVals[, which(lobarValues == v)],1,function(x) sum(x, na.rm = T))
tmpOut <- cbind(tmpOut, sumVal)
}
vol.lobes <- tmpOut

# Now do it for all other modalities
initVals <- c(223, 341, 440, 572, 702, 1221, 1338)
initMods <- c("ct", "gmd", "cortcon", "cbf", "tr", "alff", "reho")
allVals <- NULL
for(p in 1:length(initVals)){
  tmp.seq <- seq(initVals[p], initVals[p]+97)
  tmp.dat <- allData[,tmp.seq]
  tmpOut <- NULL
  # Now produce the lobar values just like we did for the volume values, only these have to be volume weighted
  for(v in names(table(lobarValues))){
    tmp.dat.2 <- tmp.dat[,which(lobarValues == v)]
    tmp.dat.vol <- tmpVolVals[, which(lobarValues == v)]
    tmpOutVals <- NULL
    for(z in 1:1601){
      sqw <- weighted.mean(tmp.dat.2[z,], tmp.dat.vol[z,], na.rm=T)
      tmpOutVals <- append(tmpOutVals, sqw)
    }
    tmpOut <- cbind(tmpOut, tmpOutVals)
  }
  allVals <- cbind(allVals, tmpOut)
}
# Now prepare the entire fil
allOut <- cbind(vol.lobes, allVals)

# Now prepare the column names
templatVals <- c("%MODALITY%_jlfLobe_%MEASURE%_R_Limbic_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Limbic_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Insular_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Insular_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Frontal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Frontal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Parietal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Parietal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Occipital_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Occipital_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Temporal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Temporal_Lobe")
modVal <- c("mprage", "mprage", "mprage", "mprage", "pcasl", "dti", "rest", "rest")
measureVal <- c("vol", "ct", "gmd", "cortcon", "cbf", "tr", "alff", "reho")
colnames(allOut)[1:12] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOut)[13:24] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])
colnames(allOut)[25:36] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])
colnames(allOut)[37:48] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])
colnames(allOut)[49:60] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])
colnames(allOut)[61:72] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])
colnames(allOut)[73:84] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[7]), pattern="%MEASURE%", replacement=measureVal[7])
colnames(allOut)[85:96] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[8]), pattern="%MEASURE%", replacement=measureVal[8])

# Now attach it to all of the imaging data
allData <- cbind(allData, allOut)
allOut <- cbind(allData$bblid, allData$scanid, allOut)
write.csv(allOut, "~/tmpVals.csv", quote=F, row.names=F, na="")

# Now attach the cognitive data
modal.scores <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/cogData2017/20170308/CNB_Factor_Scores_GO1-GO2-GO3.csv',header=TRUE)
modal.scores <- modal.scores[which(modal.scores$timepoint==1),]
hand.values <- read.csv('/home/tymoore/GO1_Handedness.csv')
race.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')
ses.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n9498_go1_environment_factor_scores_tymoore_20150909.csv')
demo.data <- merge(modal.scores, hand.values, intersect(names(modal.scores), names(hand.values)))
demo.data <- merge(demo.data, race.vals, by=intersect(names(demo.data), names(race.vals)))
demo.data <- merge(demo.data, ses.vals, by=intersect(names(demo.data), names(ses.vals)))
all.data <- merge(demo.data, allData, by=intersect(names(demo.data), names(allData)), all=T)
all.data <- all.data[all.data$bblid %in% allData$bblid,]
fileName <- paste('/home/adrose/forRuben/data/n1601_imagingDataDump_', Sys.Date(), '.csv', sep='')
write.csv(all.data, fileName, quote=F, row.names=F)

# Now limit it to only the subjects who we use in the hi Lo analysis
data.values <- read.csv('/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv')
health.values <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/healthData/healthexclude_ltn.csv')
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
# Find the subjects that had their cnb within one year of their imaging session
scan.Value <- data.values$ageAtGo1Scan
cnb.values <- data.values$ageAtGo1Cnb
diff.values <- scan.Value - cnb.values
diff.values <-  rep(0, length(diff.values))
acceptable.subjs <- which(diff.values <=12)
bblid.index <- data.values$bblid[acceptable.subjs]
scanid.index <- data.values$scanid[acceptable.subjs]

# Now limit this bblid index to those that also pass the health exclusions 
bblid.index <- bblid.index[bblid.index %in% health.values$bblid[which(health.values$incidentalExclude==0)]]
bblid.index <- bblid.index[bblid.index %in% volume.data$bblid[which(volume.data$t1Exclude==0)]]
all.data.hilo <- all.data[all.data$bblid %in% bblid.index,]
fileName <- paste('/home/adrose/forRuben/data/n1601_hiLoDataDump_', Sys.Date(), '.csv', sep='')
write.csv(all.data.hilo, fileName, quote=F, row.names=F, na="")

#### Now do the n2416 data
#### Dis gon be gud
# load all data
basePath <- '/home/adrose/dataPrepForHiLoPaper/data/rawData2416/n2416_'
# Create a loop to load, and merge all data
allData <- read.csv(paste(basePath, 'antsCtVol_jlfVol.csv', sep=''))
allData <- allData[,-grep('Cerebral_White_Matter', names(allData))]
allData[which(allData$t1Exclude==1),grep('mprage_jlf_vol_', names(allData))] <- NA

suffixVals <- c('jlfCt.csv','jlfGMD.csv','jlfCc.csv','jlfCbf-Impute.csv','jlfTR.csv','jlfRD.csv','jlfAD.csv','jlfFA.csv','jlfAlff.csv','jlfReho.csv','jhuTRTracts.csv','jhuRDTracts.csv','jhuADTracts.csv','jhuFATracts.csv')
exclusionVals <- c('t1Exclude', 't1Exclude', 't1Exclude', 'pcaslExclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude','restExclude', 'restExclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude')
grepVals <- c('mprage_jlf_', 'mprage_jlf_', 'mprage_jlf', 'pcasl_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'rest_jlf', 'rest_jlf', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_')
binaryVal <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(z in 1:length(suffixVals)){
  # Now apply the exclusion
  i <- suffixVals[z] 
  e <- exclusionVals[z]
  bV <- binaryVal[z]
  g <- grepVals[z]
  tmp <- read.csv(paste(basePath, i, sep=''))
  print(paste(z,dim(tmp)))
  tmp[which(tmp[e]!=bV),grep(g, names(tmp))] <- NA
  if(z > 4){
    checkGrep <- grep('t1Exclude', names(tmp))
    if(length(checkGrep) > 0 ){
      tmp <- tmp[,-checkGrep]
    }
  }  
  allData <- merge(allData, tmp, by=intersect(names(allData), names(tmp)), all=T)
  print(dim(allData))
}
allData <- allData[!duplicated(allData),]

# Now create our summary metrics
# These will be weighted averages by volume across the entire brain
volData <- allData[,c(1,2,grep('mprage_jlf_vol_', names(allData)))]
ctVals <- allData[,c(1,2,grep('mprage_jlf_ct_', names(allData)))]
cbfVals <- allData[,c(1,2,grep('pcasl_jlf_cbf', names(allData)))]
cbfVals <- cbfVals[,-grep('_Cerebral_White_Matter', names(cbfVals))]
cbfWMVals <- cbfVals[,c(1,2,116:127)]
rehoVals <- allData[,c(1,2,grep('rest_jlf_reho_', names(allData)))]
alffVals <- allData[,c(1,2,grep('rest_jlf_alff_', names(allData)))]
trVals <- allData[,c(1,2,grep('dti_jlf_tr_', names(allData)))]
trVals <- trVals[,-grep('_MeanTR', names(trVals))]
trWMVals <- trVals[,c(1,2,7,121:132)]
faVals <- allData[,c(1,2,grep('dti_jlf_fa_', names(allData)))]
faWMVals <- faVals[,c(1,2,7,120:131)]
dataValue <- c('ctVals', 'cbfVals', 'rehoVals', 'alffVals', 'trVals', 'cbfWMVals', 'trWMVals','faWMVals')
nameRep <- c('mprage_jlf_ct_','pcasl_jlf_cbf_','rest_jlf_reho_','rest_jlf_alff_','dti_jlf_tr_','pcasl_jlf_cbf_','dti_jlf_tr_','dti_jlf_fa_')
output <- NULL
for(q in 1:length(dataValue)){
  tmpDat <- get(dataValue[q])
  volNames <- gsub(names(volData), pattern='mprage_jlf_vol_', replacement='')
  tmpNames <- gsub(names(tmpDat), pattern=nameRep[q], replacement='') 
  tmpVol <- volData[,volNames %in% tmpNames] 
  print(dim(tmpVol))
  print(dim(tmpDat))
  tmpOutVals <- NULL
  for(z in 1:1601){
    weightedVal <- weighted.mean(tmpDat[z,-c(1,2)],tmpVol[z,-c(1,2)], na.rm=T)
    tmpOutVals <- append(tmpOutVals, weightedVal)
  }
  output <- cbind(output, tmpOutVals)
}

## Now produce a TBV value
## This will include all of the parenchyma
## we will also produce a total GM and WM values here. 
## I am going to call the brain stem a WM ROI
TBVValsOut <- volData[,c(1,2,5:15, 17,18,23:141)]
TBVValsOut <- apply(TBVValsOut[,-c(1,2)], 1, sum)
GMValsOut <- volData[,c(1,2,5,6,10,11,12,13,17,18,23:129)]
GMValsOut <- apply(GMValsOut[,-c(1,2)], 1, sum)
WMValsOut <- volData[,c(1,2,9,14,15,130:141)]
WMValsOut <- apply(WMValsOut[,-c(1,2)], 1, sum)

# Now I need to do the same with just the GM regions
output <- cbind(output, allData$pcaslMeanGMValue,allData$dti_jlf_tr_MeanTR, TBVValsOut, GMValsOut, WMValsOut)
colnames(output) <- c('mprage_jlf_ct_MeanCT', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF', 'dti_jlf_tr_MeanWholeBrainTR', 'pcasl_jlf_cbf_MeanGMCBF', 'dti_jlf_tr_MeanGMTR','pcasl_jlf_cbf_MeanWMCBF','dti_jlf_tr_MeanWMTR','dti_jlf_fa_MeanWMFA','mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM')
allData <- cbind(allData, output)

# Now produce lobar values
# Start with just volume
lobarValues <- c(100, 101, 102, 103, 104, 105, 106, 107, 108,109, 104, 105, 108, 109, 100, 101, 104, 105, 104, 105, 122,123, 104, 105, 108, 109, 122, 123, 108, 109, 104, 105, 100, 101, 104, 105, 104, 105, 108, 109, 104, 105, 106, 107, 104, 105, 104, 105, 122, 123, 108, 109, 108, 109, 104, 105, 104, 105, 100, 101, 106, 107, 100, 101, 102, 103, 106, 107, 106, 107, 104, 105, 122, 123, 104, 105, 104, 105, 104, 105, 106, 107, 108, 109, 106, 107, 122, 123, 122, 123, 104, 105, 122, 123)
volVals <- 104:201
tmpVolVals <- allData[, volVals]
tmpOut <- NULL
for (v in names(table(lobarValues))){
sumVal <- apply(tmpVolVals[, which(lobarValues == v)],1,function(x) sum(x, na.rm = T))
tmpOut <- cbind(tmpOut, sumVal)
}
vol.lobes <- tmpOut

# Now do it for all other modalities
initVals <- c(223, 341, 440, 572, 702, 1221, 1338)
initMods <- c("ct", "gmd", "cortcon", "cbf", "tr", "alff", "reho")
allVals <- NULL
for(p in 1:length(initVals)){
  tmp.seq <- seq(initVals[p], initVals[p]+97)
  tmp.dat <- allData[,tmp.seq]
  tmpOut <- NULL
  # Now produce the lobar values just like we did for the volume values, only these have to be volume weighted
  for(v in names(table(lobarValues))){
    tmp.dat.2 <- tmp.dat[,which(lobarValues == v)]
    tmp.dat.vol <- tmpVolVals[, which(lobarValues == v)]
    tmpOutVals <- NULL
    for(z in 1:2416){
      sqw <- weighted.mean(tmp.dat.2[z,], tmp.dat.vol[z,], na.rm=T)
      tmpOutVals <- append(tmpOutVals, sqw)
    }
    tmpOut <- cbind(tmpOut, tmpOutVals)
  }
  allVals <- cbind(allVals, tmpOut)
}
# Now prepare the entire fil
allOut <- cbind(vol.lobes, allVals)

# Now prepare the column names
templatVals <- c("%MODALITY%_jlfLobe_%MEASURE%_R_Limbic_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Limbic_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Insular_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Insular_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Frontal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Frontal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Parietal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Parietal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Occipital_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Occipital_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_R_Temporal_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_Temporal_Lobe")
modVal <- c("mprage", "mprage", "mprage", "mprage", "pcasl", "dti", "rest", "rest")
measureVal <- c("vol", "ct", "gmd", "cortcon", "cbf", "tr", "alff", "reho")
colnames(allOut)[1:12] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOut)[13:24] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])
colnames(allOut)[25:36] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])
colnames(allOut)[37:48] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])
colnames(allOut)[49:60] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])
colnames(allOut)[61:72] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])
colnames(allOut)[73:84] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[7]), pattern="%MEASURE%", replacement=measureVal[7])
colnames(allOut)[85:96] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[8]), pattern="%MEASURE%", replacement=measureVal[8])

# Now attach it to all of the imaging data
allData <- cbind(allData, allOut)
allOut <- cbind(allData$bblid, allData$scanid, allOut)

# Now attach the cognitive data
# Grab the cognitive data from the already prepped data 
# from the dataPrep.R script
# This will save on the multiple id nightmare I once faced...
tmpDatWithCog <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg2416/volumeData.csv')
tmpDatWithCog <- tmpDatWithCog[,1:29]
longLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/pnc_diagnosis_categorical_20170526.csv')
tmpDatWithCog <- merge(tmpDatWithCog, longLabels, all=T)
all.data <- merge(allData, tmpDatWithCog, by=c('bblid', 'scanid'), all=T)
all.data <- all.data[paste(all.data$bblid, all.data$scanid) %in% paste(allData$bblid, allData$scanid),]
fileName <- paste('/home/adrose/forRuben/data/n2416_imagingDataDump_', Sys.Date(), '.csv', sep='')
write.csv(all.data, fileName, quote=F, row.names=F)
