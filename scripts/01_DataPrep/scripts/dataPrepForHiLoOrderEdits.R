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
#data.values <- read.csv('/home/analysis/redcap_data/201602/go1/n1601_go1_datarel_020716.csv')
data.values <- read.csv('/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv')
modal.scores <- read.csv('/home/tymoore/GO1_GO2_CNB_Factor_Scores.csv',header=TRUE)
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCt.csv')
rest.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/REST_QA.csv')


# Now create a really quick freesurfer volumes df
tmp <- merge(data.values, volume.data, by=c('bblid', 'scanid'))
attach(tmp)
fs.values <- cbind(bblid, scanid, t1Exclude, mpragefsFinalExclude, data.values[,106:175])
fs.values <- cbind(bblid, scanid, t1Exclude, mpragefsFinalExclude, tmp[,2296:2363], tmp[,2500:2544])
fs.values.seg <- cbind(bblid, scanid, t1Exclude, mpragefsFinalExclude, tmp[,2500:2544])
detach(tmp)
rm(tmp)

# Now limit the reho data to the analysis' that we are interested in
subj.rest.data.cols <- seq(1,28)
reho.data.cols <- seq(331, 481)
alff.data.cols <- seq(29,179)
reho.cols <- c(subj.rest.data.cols, reho.data.cols)
alff.cols <- c(subj.rest.data.cols, alff.data.cols)
reho.data <- rest.data[,reho.cols]
alff.data <- rest.data[,alff.cols]

# Now fix the scan id column for our rest data
reho.data$scanid <- strSplitMatrixReturn(reho.data$scanid, 'x')[,2]
alff.data$scanid <- strSplitMatrixReturn(alff.data$scanid, 'x')[,2]

# First thing I need to do is get an index of subjects
# that had their CNB wihtin one year of their scan
scan.Value <- data.values$ageAtGo1Scan
cnb.values <- data.values$ageAtGo1Cnb
diff.values <- scan.Value - cnb.values
acceptable.subjs <- which(diff.values <=12)
bblid.index <- data.values$bblid[acceptable.subjs]
scanid.index <- data.values$scanid[acceptable.subjs]

## Now limit our data values to acceptable subjects 
# First apply the cnb restriction
volume.data <- volume.data[volume.data$bblid %in% bblid.index,]
cbf.data <- cbf.data[cbf.data$bblid %in% bblid.index,]
gmd.data <- gmd.data[gmd.data$bblid %in% bblid.index,]
ct.data <- ct.data[ct.data$bblid %in% bblid.index,]
fs.values <- fs.values[fs.values$bblid %in% bblid.index,]
reho.data <- reho.data[reho.data$bblid %in% bblid.index,]
alff.data <- alff.data[alff.data$bblid %in% bblid.index,]

# Now apply any data quality restrictions 
volume.data <- volume.data[which(volume.data$t1Exclude!=1),]
cbf.data <- cbf.data[which(cbf.data$pcaslExclude!=1),]
gmd.data <- gmd.data[which(gmd.data$t1Exclude!=1),]
ct.data <- ct.data[which(ct.data$t1Exclude!=1),]
fs.values <- fs.values[which(fs.values$t1Exclude==0 & fs.values$mpragefsFinalExclude==0),]
fs.values <- fs.values[complete.cases(fs.values),]
reho.data <- reho.data[which(reho.data$restExclude==0),]
alff.data <- alff.data[which(alff.data$restExclude==0),]

# Now lets attach the factor scrores
modal.scores <- modal.scores[which(modal.scores$Visit==1),]
volume.data <- merge(modal.scores, volume.data, by='bblid')
cbf.data <- merge(modal.scores, cbf.data, by='bblid')
gmd.data <- merge(modal.scores, gmd.data, by='bblid')
ct.data <- merge(modal.scores, ct.data, by='bblid')
fs.values <- merge(modal.scores, fs.values, by='bblid')
reho.data <- merge(modal.scores, reho.data, by='bblid')
alff.data <- merge(modal.scores, alff.data, by='bblid')

# now attach sex and age
volume.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(volume.data$bblid, data.values$bblid)]
volume.data$sex <- data.values$sex[match(volume.data$bblid, data.values$bblid)]
cbf.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(cbf.data$bblid, data.values$bblid)]
cbf.data$sex <- data.values$sex[match(cbf.data$bblid, data.values$bblid)]
gmd.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(gmd.data$bblid, data.values$bblid)]
gmd.data$sex <- data.values$sex[match(gmd.data$bblid, data.values$bblid)]
ct.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(ct.data$bblid, data.values$bblid)]
ct.data$sex <- data.values$sex[match(ct.data$bblid, data.values$bblid)]
fs.values$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(fs.values$bblid, data.values$bblid)]
fs.values$sex <- data.values$sex[match(fs.values$bblid, data.values$bblid)]
reho.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(reho.data$bblid, data.values$bblid)]
reho.data$sex <- data.values$sex[match(reho.data$bblid, data.values$bblid)]
alff.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(alff.data$bblid, data.values$bblid)]
alff.data$sex <- data.values$sex[match(alff.data$bblid, data.values$bblid)]

# Now age and volume regress the data 
gmd.data <- regressOutVolumeAndAge(volume.data, gmd.data, 'mprage_jlf_gmd')
ct.data <- regressOutVolumeAndAge(volume.data, ct.data, 'mprage_jlf_ct')

# Now super quickly rm extra cbf areas aslo restbold regions
namesToRm <- c('Ventral_DC', 'Lat_Vent', 'Inf_Lat_Vent', 'Cerebellum_White_Matter', 
                'Cerebellum_Exterior', 'OpticChiasm','CSF', 'Ventricle', 'Brain_Stem',
                'CerVerLob')
colsToRm <- NULL
for(value in namesToRm){
  valuesToRm <- grep(value, names(cbf.data))
  colsToRm <- append(colsToRm, valuesToRm)
}
#cbf.data <- cbf.data[,-colsToRm]
cbf.data[,35:158] <- apply(cbf.data[,35:158], 2, function(x) regressOutAge(x, cbf.data$ageAtGo1Scan))

namesToRm <- c('Ventral_DC', 'Lat_Vent', 'Inf_Lat_Vent', 'Cerebellum_White_Matter', 
                'Cerebellum_Exterior', 'OpticChiasm','CSF', 'Ventricle', 'Brain_Stem',
                'CerVerLob','fornix', 'antlimb_InC', 'postlimbcerebr', 'corpus', 'WM')
colsToRm <- NULL
for(value in namesToRm){
  valuesToRm <- grep(value, names(reho.data))
  colsToRm <- append(colsToRm, valuesToRm)  
}
reho.data <- reho.data[,-colsToRm]
alff.data <- alff.data[,-colsToRm]
reho.data <- regressOutVolumeAndAge(volume.data, reho.data, 'restbold_jlf_reho')
alff.data <- regressOutVolumeAndAge(volume.data, alff.data, 'restbold_jlf_alff')

# Now age regress the volume data
volume.data[,26:180] <- apply(volume.data[,26:180], 2, function(x) regressOutAge(x, volume.data$ageAtGo1Scan))


# Now average across hemispheres
volume.data <- averageLeftAndRight(volume.data)
cbf.data <- averageLeftAndRight(cbf.data)
gmd.data <- averageLeftAndRight(gmd.data)
ct.data <- averageLeftAndRight(ct.data)
reho.data <- averageLeftAndRight(reho.data)
alff.data <- averageLeftAndRight(alff.data)

# Now write these csv's
write.csv(reho.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegReHo.csv', quote=F, row.names=F)
write.csv(alff.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegAlff.csv', quote=F, row.names=F)


# Now I will produce the regressed within modality data
# Regressing wihtin modality is used to make the output 
# of the final variable selection linear model interperatble
# These values should not be used for anything excpet the final vairbale selection and model
volume.data <- regressWithinModality(volume.data, 'mprage_jlf_vol')
cbf.data <- regressWithinModality(cbf.data, 'pcasl_jlf_cbf')
gmd.data <- regressWithinModality(gmd.data, 'mprage_jlf_gmd')
ct.data <- regressWithinModality(ct.data, 'mprage_jlf_ct')
reho.data <- regressWithinModality(reho.data, 'restbold_jlf_reho')
alff.data <- regressWithinModality(alff.data, 'restbold_jlf_alff')

# Now write out the csv's 
write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgressionAndWithinModalRegression/meanLRAgeRegModRegVOL.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgressionAndWithinModalRegression/meanLRAgeRegModRegCBF.csv', quote=F, row.names=F)
write.csv(reho.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgressionAndWithinModalRegression/meanLRAgeRegModRegReho.csv', quote=F, row.names=F)
write.csv(alff.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgressionAndWithinModalRegression/meanLRAgeRegModRegAlff.csv', quote=F, row.names=F)
