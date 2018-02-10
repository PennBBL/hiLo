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
rehoVals <- allData[,c(1,2,grep('rest_jlf_reho_', names(allData)))]
alffVals <- allData[,c(1,2,grep('rest_jlf_alff_', names(allData)))]
trVals <- allData[,c(1,2,grep('dti_jlf_tr_', names(allData)))]
trVals <- trVals[,-grep('_MeanTR', names(trVals))]
dataValue <- c('ctVals', 'cbfVals', 'rehoVals', 'alffVals', 'trVals')
nameRep <- c('mprage_jlf_ct_', 'pcasl_jlf_cbf_', 'rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_tr_')
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

# Now I need to do the same with just the GM regions
output <- cbind(output, allData$pcaslMeanGMValue,allData$dti_jlf_tr_MeanTR)
colnames(output) <- c('mprage_jlf_ct_MeanCT', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF', 'dti_jlf_tr_MeanWholeBrainTR', 'pcasl_jlf_cbf_MeanGMCBF', 'dti_jlf_tr_MeanGMTR')
allData <- cbind(allData, output)

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
fileName <- paste('/home/adrose/forRuben/data/n1601_imagingDataDump_', Sys.Date(), sep='')
write.csv(all.data, fileName, quote=F, row.names=F)

# Now limit it to only the subjects who we use in the hi Lo analysis
data.values <- read.csv('/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv')
health.values <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/healthData/healthexclude_ltn.csv')
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
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
all.data.hilo <- all.data[all.data$bblid %in% bblid.index,]
fileName <- paste('/home/adrose/forRuben/data/n1601_hiLoDataDump_', Sys.Date(), sep='')
write.csv(all.data.hilo, fileName, quote=F, row.names=F)
