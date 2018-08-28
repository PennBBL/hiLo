library('reshape2')
source('../functions/functions.R')

# load all data
basePath <- '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_'
# Create a loop to load, and merge all data
allData <- read.csv(paste(basePath, 'antsCtVol_jlfVol.csv', sep=''))
allData <- allData[,-grep('Cerebral_White_Matter', names(allData))]
allData[which(allData$t1Exclude==1),grep('mprage_jlf_vol_', names(allData))] <- NA

suffixVals <- c('jlfCt.csv','jlfGMD.csv','jlfCc.csv','jlfCbf-Impute.csv','jlfTR.csv','jlfRD.csv','jlfAD.csv','jlfFA.csv','jlfAlff.csv','jlfReho.csv','jhuTRTracts.csv','jhuRDTracts.csv','jhuADTracts.csv','jhuFATracts.csv','jhuTRLabels.csv', 'jhuRDLabels.csv','jhuADLabels.csv','jhuFALabels.csv')
exclusionVals <- c('t1Exclude', 't1Exclude', 't1Exclude', 'pcaslExclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude', 'dti64Exclude','restExclude', 'restExclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude','dti64Exclude')
grepVals <- c('mprage_jlf_', 'mprage_jlf_', 'mprage_jlf', 'pcasl_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'rest_jlf', 'rest_jlf', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_')
binaryVal <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
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
outputNames <- c('mprage_jlf_ct_MeanCT', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF', 'dti_jlf_tr_MeanWholeBrainTR', 'pcasl_jlf_cbf_MeanWMCBF', 'dti_jlf_tr_MeanWMTR', 'dti_jlf_fa_MeanWMFA')
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
  colnames(output)[q] <- outputNames[q]
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
colnames(output)[9:13] <- c('pcasl_jlf_cbf_MeanGMCBF','dti_jlf_tr_MeanWholeBrainTR','mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM')
allData <- cbind(allData, output)

# Now produce lobar values
# Start with just volume
lobarValues <- c(100,101,102,103,104,105,106,107,108,109,104,105,108,109,100,101,104,105,104,105,122,123,104,105,108,109,122,123,108,109,104,105,100,101,104,105,104,105,108,109,104,105,106,107,104,105,104,105,122,123,108,109,108,109,104,105,104,105,100,101,106,107,100,101,102,103,106,107,106,107,104,105,122,123,104,105,122,123,104,105,104,105,104,105,106,107,108,109,106,107,122,123,122,123,104,105,122,123)
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

## Now I need to do DGM lobular values
lobarVals <- rep(c(1,2),7)
dgmVals <- c(77:80,82:83,89:90,95:100)
tmpVolVals <- allData[,dgmVals]
tmpOut <- NULL
for(v in names(table(lobarVals))){
  sumVal <- apply(tmpVolVals[, which(lobarVals == v)],1,function(x) sum(x, na.rm = T))
  tmpOut <- cbind(tmpOut, sumVal)
}
vol.lobes.dgm <- tmpOut

## Now go through all of our modalities with DGM values
## and calculate the L and R DGM values
initVal <- list()
initVal[[1]] <- c(321,322,323,323,326,327,330,331,332,333,334,335,336,337)
initVal[[2]] <- c(555,556,557,558,560,561,564,565,566,567,568,569,570,571)
initVal[[3]] <- c(682,683,684,685,687,688,691,692,693,694,695,696,697,698)
initVal[[4]] <- c(1202,1203,1204,1205,1206,1207,1210,1211,1212,1213,1214,1215,1216,1217)
initVal[[5]] <- c(1319,1320,1321,1322,1323,1324,1327,1328,1329,1330,1331,1332,1333,1334)
allValsDGM <- NULL
for(p in 1:length(initVal)){
  tmp.dat <- allData[,initVal[[p]]]
  tmpOut <- NULL
  for(v in names(table(lobarVals))){
    tmp.dat.2 <- tmp.dat[,which(lobarVals==v)]
    tmp.dat.vol <- tmpVolVals[,which(lobarVals==v)]
    tmpOutVals <- NULL
    for(z in 1:1601){
      sqw <- weighted.mean(tmp.dat.2[z,], tmp.dat.vol[z,], na.rm=T)
      tmpOutVals <- append(tmpOutVals, sqw)
    }
    tmpOut <- cbind(tmpOut, tmpOutVals)
  }
  allValsDGM <- cbind(allValsDGM, tmpOut)
}
allOutDGM <- cbind(vol.lobes.dgm, allValsDGM)

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

## Now do our DGM lobes
templatVals <- c("%MODALITY%_jlfLobe_%MEASURE%_R_DGM_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_DGM_Lobe")
modVal <- c("mprage","mprage", "pcasl", "dti", "rest", "rest")
measureVal <- c("vol","gmd","cbf", "tr", "alff", "reho")
colnames(allOutDGM)[1:2] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOutDGM)[3:4] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])
colnames(allOutDGM)[5:6] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])
colnames(allOutDGM)[7:8] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])
colnames(allOutDGM)[9:10] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])
colnames(allOutDGM)[11:12] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])

## Now combine all of our lobe values into one matrix
allOut <- cbind(allOut[,1:12],allOutDGM[,1:2],allOut[,13:24],allOut[,25:36],allOutDGM[,3:4],allOut[,37:48],allOut[,49:60],allOutDGM[,5:6],allOut[,61:72],allOutDGM[,7:8],allOut[,73:84],allOutDGM[,9:10],allOut[,85:96], allOutDGM[,11:12])

# Now attach it to all of the imaging data
allData <- cbind(allData, allOut)
allOut <- cbind(allData$bblid, allData$scanid, allOut)
#write.csv(allOut, "~/tmpVals.csv", quote=F, row.names=F, na="")

## Now do the hi lo labels
# First isolate the volume regions
volVals <- grep('mprage_jlf_vol_', names(allData))
tmpVolVals <- allData[, volVals]
## Now create an array with the lobe indicies
tmp <- colnames(tmpVolVals)
dim(tmp) <- c(143,1)
lobarValues <- apply(tmp, 1, findLobe)
# Now go through and calculate our volume for each of these values
tmpOut <- NULL
for(v in names(table(lobarValues))[1:9]){
  sumVal <- apply(tmpVolVals[,which(lobarValues == v)], 1, function(x) sum(x, na.rm=T))
  sumVal[sumVal==0] <- NA
  tmpOut <- cbind(tmpOut, sumVal)
}
vol.lobes <- tmpOut

# Now do it for all other regions w/o subcortical areas
grepVals <- c('mprage_jlf_gmd_','mprage_jlf_ct_', 'mprage_jlf_cortcon_', 'pcasl_jlf_cbf_', 'dti_jlf_tr_', 'rest_jlf_alff_', 'rest_jlf_reho_')
allVals <- NULL
for(i in grepVals){
  # Now declare our dataset, and find the ROI's to use
  tmpData <- grep(i, names(allData))
  tmpData <- allData[,tmpData]
  tmp <- colnames(tmpData)
  dim(tmp) <- c(length(tmp), 1)
  lobarValues <- apply(tmp, 1, findLobe)
  lobarValuesNames  <- head(names(table(lobarValues)), -1)
  print(lobarValuesNames)
  ## Now we need to go through each lobar value
  ## and find the weighted mean across all of the respective regions
  tmpOut <- NULL
  for(v in lobarValuesNames){
    tmp.dat.2 <- tmpData[,which(lobarValues==v)]
    tmpVolVals2 <- tmpVolVals[,which(lobarValues==v)]
    tmpOutVals <- NULL
    for(z in 1:1601){
      sqw <- weighted.mean(tmp.dat.2[z,], tmpVolVals2[z,], na.rm=T)
      tmpOutVals <- append(tmpOutVals, sqw)
    }
    tmpOut <- cbind(tmpOut, tmpOutVals)
  }
  allVals <- cbind(allVals, tmpOut)
}
allOut <- cbind(vol.lobes, allVals)
## Now name our columns
# Now prepare the column names
templatVals <- c("%MODALITY%_jlfHiLoLobe_%MEASURE%_Basal_Ganglia",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Limbic",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Frontal_Orbital",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Frontal_Dorsal",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Temporal",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Parietal",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Occipital",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Cerebellum",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_White_Matter")
modVal <- c("mprage", "mprage", "mprage", "mprage", "pcasl", "dti", "rest", "rest")
measureVal <- c("vol", "gmd", "ct", "cortcon", "cbf", "tr", "alff", "reho")
colnames(allOut)[1:9] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOut)[10:17] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])[1:8]
colnames(allOut)[18:23] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])[2:7]
colnames(allOut)[24:28] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])[2:6]
colnames(allOut)[29:36] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])[-8]
colnames(allOut)[37:45] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])
colnames(allOut)[46:53] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[7]), pattern="%MEASURE%", replacement=measureVal[7])[-9]
colnames(allOut)[54:61] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[8]), pattern="%MEASURE%", replacement=measureVal[8])[-9]

# Now attach these to the entire data frame
allData <- cbind(allData, allOut)

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
fileName <- paste('/home/adrose/forRuben/data/tmp/n1601_imagingDataDump_', Sys.Date(), '.csv', sep='')
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
fileName <- paste('/home/adrose/forRuben/data/tmp/n1601_hiLoDataDump_', Sys.Date(), '.csv', sep='')
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
  # Now merge the health exclude values
  tmp <- merge(tmp, health.values, all=T)
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
trVals <- allData[,c(1,2,grep('dti64_jlf_tr_', names(allData)))]
trWMVals <- trVals[,c(1,2,7,121:132)]
faVals <- allData[,c(1,2,grep('dti64_jlf_fa_', names(allData)))]
faWMVals <- faVals[,c(1,2,7,120:131)]
dataValue <- c('ctVals', 'cbfVals', 'rehoVals', 'alffVals', 'trVals', 'cbfWMVals', 'trWMVals','faWMVals')
nameRep <- c('mprage_jlf_ct_','pcasl_jlf_cbf_','rest_jlf_reho_','rest_jlf_alff_','dti64_jlf_tr_','pcasl_jlf_cbf_','dti64_jlf_tr_','dti64_jlf_fa_')
output <- NULL
outputNames <- c('mprage_jlf_ct_MeanCT', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF', 'dti_jlf_tr_MeanWholeBrainTR', 'pcasl_jlf_cbf_MeanWMCBF', 'dti_jlf_tr_MeanWMTR', 'dti_jlf_fa_MeanWMFA')
for(q in 1:length(dataValue)){
  tmpDat <- get(dataValue[q])
  volNames <- gsub(names(volData), pattern='mprage_jlf_vol_', replacement='')
  tmpNames <- gsub(names(tmpDat), pattern=nameRep[q], replacement='') 
  tmpVol <- volData[,volNames %in% tmpNames] 
  print(dim(tmpVol))
  print(dim(tmpDat))
  tmpOutVals <- NULL
  for(z in 1:2416){
    weightedVal <- weighted.mean(tmpDat[z,-c(1,2)],tmpVol[z,-c(1,2)], na.rm=T)
    tmpOutVals <- append(tmpOutVals, weightedVal)
  }
  output <- cbind(output, tmpOutVals)
  colnames(output)[q] <- outputNames[q]
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
output <- cbind(output, allData$pcaslMeanGMValue,TBVValsOut, GMValsOut, WMValsOut)
colnames(output)[9:12] <- c('pcasl_jlf_cbf_MeanGMCBF','mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM')
allData <- cbind(allData, output)

# Now produce lobar values
# Start with just volume
lobarValues <- c(100,101,102,103,104,105,106,107,108,109,104,105,108,109,100,101,104,105,104,105,122,123,104,105,108,109,122,123,108,109,104,105,100,101,104,105,104,105,108,109,104,105,106,107,104,105,104,105,122,123,108,109,108,109,104,105,104,105,100,101,106,107,100,101,102,103,106,107,106,107,104,105,122,123,104,105,122,123,104,105,104,105,104,105,106,107,108,109,106,107,122,123,122,123,104,105,122,123)
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

## Now I need to do DGM lobular values
lobarVals <- rep(c(1,2),7)
dgmVals <- c(88:91,93:94,100:101,106:111)
tmpVolVals <- allData[,dgmVals]
tmpOut <- NULL
for(v in names(table(lobarVals))){
  sumVal <- apply(tmpVolVals[, which(lobarVals == v)],1,function(x) sum(x, na.rm = T))
  tmpOut <- cbind(tmpOut, sumVal)
}
vol.lobes.dgm <- tmpOut

## Now go through all of our modalities with DGM values
## and calculate the L and R DGM values
initVal <- list()
initVal[[1]] <- c(332,333,334,335,337,338,341,342,343,344,345,346,347,348)
initVal[[2]] <- c(566,567,568,569,571,572,575,576,577,578,579,580,581,582)
initVal[[3]] <- c(693,694,695,696,698,699,702,703,704,705,706,707,708,709)
initVal[[4]] <- c(1213,1214,1215,1216,1217,1218,1221,1222,1223,1224,1225,1226,1227,1228)
initVal[[5]] <- c(1330,1331,1332,1333,1334,1335,1338,1339,1340,1341,1342,1343,1344,1335)
allValsDGM <- NULL
for(p in 1:length(initVal)){
  tmp.dat <- allData[,initVal[[p]]]
  tmpOut <- NULL
  for(v in names(table(lobarVals))){
    tmp.dat.2 <- tmp.dat[,which(lobarVals==v)]
    tmp.dat.vol <- tmpVolVals[,which(lobarVals==v)]
    tmpOutVals <- NULL
    for(z in 1:2416){
      sqw <- weighted.mean(tmp.dat.2[z,], tmp.dat.vol[z,], na.rm=T)
      tmpOutVals <- append(tmpOutVals, sqw)
    }
    tmpOut <- cbind(tmpOut, tmpOutVals)
  }
  allValsDGM <- cbind(allValsDGM, tmpOut)
}
allOutDGM <- cbind(vol.lobes.dgm, allValsDGM)

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
modVal <- c("mprage", "mprage", "mprage", "mprage", "pcasl", "dti64", "rest", "rest")
measureVal <- c("vol", "ct", "gmd", "cortcon", "cbf", "tr", "alff", "reho")
colnames(allOut)[1:12] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOut)[13:24] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])
colnames(allOut)[25:36] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])
colnames(allOut)[37:48] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])
colnames(allOut)[49:60] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])
colnames(allOut)[61:72] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])
colnames(allOut)[73:84] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[7]), pattern="%MEASURE%", replacement=measureVal[7])
colnames(allOut)[85:96] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[8]), pattern="%MEASURE%", replacement=measureVal[8])

## Now do our DGM lobes
templatVals <- c("%MODALITY%_jlfLobe_%MEASURE%_R_DGM_Lobe",
"%MODALITY%_jlfLobe_%MEASURE%_L_DGM_Lobe")
modVal <- c("mprage","mprage", "pcasl", "dti", "rest", "rest")
measureVal <- c("vol","gmd","cbf", "tr", "alff", "reho")
colnames(allOutDGM)[1:2] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOutDGM)[3:4] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])
colnames(allOutDGM)[5:6] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])
colnames(allOutDGM)[7:8] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])
colnames(allOutDGM)[9:10] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])
colnames(allOutDGM)[11:12] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])

## Now combine all of our lobe values into one matrix
allOut <- cbind(allOut[,1:12],allOutDGM[,1:2],allOut[,13:24],allOut[,25:36],allOutDGM[,3:4],allOut[,37:38],allOut[,49:60],allOutDGM[,5:6],allOut[,61:72],allOutDGM[,5:6],allOut[,73:84], allOutDGM[,7:8],allOut[,85:96],allOutDGM[,9:10],allOut[,85:96], allOutDGM[,11:12])

# Now attach it to all of the imaging data
allData <- cbind(allData, allOut)
allOut <- cbind(allData$bblid, allData$scanid, allOut)

## Now do the hi lo labels
# First isolate the volume regions
volVals <- grep('mprage_jlf_vol_', names(allData))
tmpVolVals <- allData[, volVals]
## Now create an array with the lobe indicies
tmp <- colnames(tmpVolVals)
dim(tmp) <- c(143,1)
lobarValues <- apply(tmp, 1, findLobe)
# Now go through and calculate our volume for each of these values
tmpOut <- NULL
for(v in names(table(lobarValues))[1:9]){
  sumVal <- apply(tmpVolVals[,which(lobarValues == v)], 1, function(x) sum(x, na.rm=T))
  sumVal[sumVal==0] <- NA
  tmpOut <- cbind(tmpOut, sumVal)
}
vol.lobes <- tmpOut

# Now do it for all other modalities 
grepVals <- c('mprage_jlf_gmd_','mprage_jlf_ct_', 'mprage_jlf_cortcon_', 'pcasl_jlf_cbf_', 'dti64_jlf_tr_', 'rest_jlf_alff_', 'rest_jlf_reho_')
allVals <- NULL
for(i in grepVals){
  # Now declare our dataset, and find the ROI's to use
  tmpData <- grep(i, names(allData))
  tmpData <- allData[,tmpData]
  tmp <- colnames(tmpData)
  dim(tmp) <- c(length(tmp), 1)
  lobarValues <- apply(tmp, 1, findLobe)
  lobarValuesNames  <- head(names(table(lobarValues)), -1)
  print(lobarValuesNames)
  ## Now we need to go through each lobar value
  ## and find the weighted mean across all of the respective regions
  tmpOut <- NULL
  for(v in lobarValuesNames){
    tmp.dat.2 <- tmpData[,which(lobarValues==v)]
    tmpVolVals2 <- tmpVolVals[,which(lobarValues==v)]
    tmpOutVals <- NULL
    for(z in 1:2416){
      sqw <- weighted.mean(tmp.dat.2[z,], tmpVolVals2[z,], na.rm=T)
      tmpOutVals <- append(tmpOutVals, sqw)
    }
    tmpOut <- cbind(tmpOut, tmpOutVals)
  }
  allVals <- cbind(allVals, tmpOut)
}
allOut <- cbind(vol.lobes, allVals)
## Now name our columns
# Now prepare the column names
templatVals <- c("%MODALITY%_jlfHiLoLobe_%MEASURE%_Basal_Ganglia",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Limbic",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Frontal_Orbital",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Frontal_Dorsal",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Temporal",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Parietal",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Occipital",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_Cerebellum",
"%MODALITY%_jlfHiLoLobe_%MEASURE%_White_Matter")
modVal <- c("mprage", "mprage", "mprage", "mprage", "pcasl", "dti", "rest", "rest")
measureVal <- c("vol", "gmd", "ct", "cortcon", "cbf", "tr", "alff", "reho")
colnames(allOut)[1:9] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[1]), pattern="%MEASURE%", replacement=measureVal[1])
colnames(allOut)[10:17] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[2]), pattern="%MEASURE%", replacement=measureVal[2])[1:8]
colnames(allOut)[18:23] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[3]), pattern="%MEASURE%", replacement=measureVal[3])[2:7]
colnames(allOut)[24:28] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[4]), pattern="%MEASURE%", replacement=measureVal[4])[2:6]
colnames(allOut)[29:36] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[5]), pattern="%MEASURE%", replacement=measureVal[5])[-8]
colnames(allOut)[37:45] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[6]), pattern="%MEASURE%", replacement=measureVal[6])
colnames(allOut)[46:53] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[7]), pattern="%MEASURE%", replacement=measureVal[7])[-9]
colnames(allOut)[54:61] <- gsub(gsub(templatVals, pattern="%MODALITY%", replacement=modVal[8]), pattern="%MEASURE%", replacement=measureVal[8])[-9]

# Now attach the cognitive data
# Grab the cognitive data from the already prepped data 
# from the dataPrep.R script
# This will save on the multiple id nightmare I once faced...
tmpDatWithCog <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg2416/volumeData.csv')
tmpDatWithTP <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n2416_pnc_protocol_validation_params_status_20170105.csv')
tmpDatWithCog <- merge(tmpDatWithCog, tmpDatWithTP)
tmpDatWithCog <- tmpDatWithCog[,c(1:29, 189)]
longLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/pnc_diagnosis_categorical-all_20170526.csv')
longClinFacScore <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/longFacScore.csv')
tmpDatWithCog <- merge(tmpDatWithCog, longLabels, all=T)
tmpDatWithCog <- merge(tmpDatWithCog, longClinFacScore, by=c('bblid', 'timepoint'))
all.data <- merge(allData, tmpDatWithCog, by=c('bblid', 'scanid'), all=T)
all.data <- all.data[paste(all.data$bblid, all.data$scanid) %in% paste(allData$bblid, allData$scanid),]
all.data[all.data==""] <- NA
## Now add the correct demographic values
long.demo.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n2416_demographics_20170310.csv')
all.data <- merge(long.demo.vals, all.data, by=c('bblid', 'scanid'), suffixes = c("",".IncompleteVals"))
all.data <- all.data[,-grep("IncompleteVals", names(all.data))]
## Now see if we can fix those NA timepoints
na.timepoint.vals <- dcast(data=all.data, bblid~timepoint)
## Find the subjects with any NA timepoints
na.subjs <- na.timepoint.vals[which(na.timepoint.vals[,4]==1),]
# Now find all of our subjects with NA timepoints
na.vals <- dcast(data=all.data, bblid~timepoint)
na.subjs.index  <- which(na.vals[,5]==1)
# Now correct the subjects w/o TP 1 values
tp.one.vals <- na.vals[which(na.vals[,2]==0 & na.vals[,5]==1),1]
tp.one.vals <- c(tp.one.vals, na.vals[which(na.vals[,2]==0 & na.vals[,5]==2),1])[-62]
tp.two.vals <- na.vals[which(na.vals[,2]==1 & na.vals[,5]==1 & na.vals[,3]==0),1]
tp.two.vals <- append(tp.two.vals, na.vals[which(na.vals[,2]==1 & na.vals[,5]==2 & na.vals[,3]==0),1])
tp.three.vals <- na.vals[which(na.vals[,2]==1 & na.vals[,5]==1 & na.vals[,3]==1 & na.vals[,4]==0),1]
tp.three.vals <- append(tp.three.vals, '89115')
# Now we need to go through the subjects and modify the correct tp to the
# correct tp value
# I am going to do this in a loop, find the minimum scanid value
# and then modify the appropriate TP value
table(all.data$timepoint)
for(b in tp.one.vals){
    # Find the minimum scanid value
    scanidVals <- all.data[which(all.data$bblid==b),'scanid']
    minSID <- min(scanidVals)
    all.data[which(all.data$bblid==b & all.data$scanid==minSID),'timepoint'] <- 1
}
table(all.data$timepoint)

# Now do TP2
table(all.data$timepoint)
for(b in tp.two.vals){
    # Find the next scanid larger than the minimum
    scanidVals <- all.data[which(all.data$bblid==b),'scanid']
    if(length(scanidVals)==2){
      cSID <- max(scanidVals)
    }
    if(length(scanidVals)==3){
      print('foo')
      cSID <- median(scanidVals)
    }
    all.data[which(all.data==b & all.data$scanid==cSID),'timepoint'] <- 2
}
table(all.data$timepoint)

# Now do TP3
table(all.data$timepoint)
for(b in tp.three.vals){
    # Find the minimum scanid value
    scanidVals <- all.data[which(all.data$bblid==b),'scanid']
    minSID <- max(scanidVals)
    all.data[which(all.data$bblid==b & all.data$scanid==minSID),'timepoint'] <- 3
}
table(all.data$timepoint)


# Awesome now we have a more correct tp variable!
# lets check this first though...
ch.vals <- dcast(data=all.data, bblid~timepoint)

## Looks like we had a couple of nefarious subjects
## time to make some manual corrections =(
bad.bblid <- ch.vals[which(ch.vals[,5]!=0),]
all.data[which(all.data$bblid==bad.bblid[1,1] & is.na(all.data$timepoint)),'timepoint'] <- 2
all.data[which(all.data$bblid==bad.bblid[2,1] & is.na(all.data$timepoint)),'timepoint'] <- 2
all.data[which(all.data$bblid==bad.bblid[3,1] & is.na(all.data$timepoint)),'timepoint'] <- c(1,2)
all.data$tpvalue <- all.data$timepoint
all.data <- all.data[,-which(names(all.data)=='timepoint')]
## Now load the cognitive non age regressed data
cog.data.two <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/CNB_Factor_Scores_GO1-GO2-GO3_NON-AGE-REGRESSED_wdates.csv')
all.data.two <- merge(all.data, cog.data.two, all=T)
all.data.two <- all.data.two[complete.cases(all.data.two$scanid),] 
fileName <- paste('/home/adrose/forRuben/data/tmp/n2416_imagingDataDump_', Sys.Date(), '.csv', sep='')
write.csv(all.data.two, fileName, quote=F, row.names=F, , na="")
