# load all data
basePath <- '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_'
# Create a loop to load, and merge all data
allData <- read.csv(paste(basePath, 'antsCtVol_jlfVol.csv', sep=''))
allData <- allData[,-grep('Cerebral_White_Matter', names(allData))]
allData[which(allData$t1Exclude==1),grep('mprage_jlf_vol_', names(allData))] <- NA

suffixVals <- c('jlfCt.csv','jlfGMD.csv','jlfCc.csv','jlfCbf-Impute.csv','jlfTR.csv','jlfRD.csv','jlfAD.csv','jlfFA.csv','jlfAlff.csv','jlfReho.csv','jhuTRTracts.csv','jhuRDTracts.csv','jhuADTracts.csv','jhuFATracts.csv')
exclusionVals <- c('t1Exclude', 't1Exclude', 't1Exclude', 'pcaslExclude', 'include_64', 'include_64', 'include_64', 'include_64','restExclude', 'restExclude','include_64','include_64','include_64','include_64')
grepVals <- c('mprage_jlf_', 'mprage_jlf_', 'mprage_jlf', 'pcasl_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'dti_jlf', 'rest_jlf', 'rest_jlf', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_', 'dti_dtitk_')
binaryVal <- c(0,0,0,0,1,1,1,1,0,0,1,1,1,1)
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
write.csv(all.data, '/home/adrose/forRuben/data/n1601_imagingDataDump_20170901.csv', quote=F, row.names=F)

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
write.csv(all.data.hilo, '/home/adrose/forRuben/data/n1601_hiLoDataDump_20170901.csv', quote=F, row.names=F)
