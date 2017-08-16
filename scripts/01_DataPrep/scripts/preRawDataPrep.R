# AFGR October 31 2016 

# This script is going to be used to prepare the pre raw data and turn it into the raw data....
# Seems needless but is important
install_load('psych', 'sas7bdat', 'mi', 'methods', 'doParallel')

# Start with T1 values 
jlfVol <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfAntsCTIntersectionVol_20170323.csv')
jlfWmVol <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfWmVol_20170303.csv')
antsVol <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/t1/n1601_antsCtVol.csv')
jlfCt <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfAtroposIntersectionCT_20170331.csv')
jlfCC <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601CCVals.csv')
jlfGmd <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfAtroposIntersectionGMD_20170410.csv')
gmdFactor <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/GMD_Factor.csv')
t1QA <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_t1QaData_20170306.csv')

# Now merge all of the t1 data
volData <- merge(t1QA, jlfVol, by=c('bblid', 'scanid'))
volData <- merge(volData, jlfWmVol, by=c('bblid', 'scanid'))
volData <- merge(volData, antsVol, by=c('bblid', 'scanid'))

# Now write our csv
write.csv(volData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv', quote=F, row.names=F)

# Now do the GMD and CT and cortcon
gmdData <- merge(t1QA, jlfGmd, by=c('bblid','scanid'))
gmdData <- merge(gmdData, gmdFactor, by='bblid')
#write csv...
write.csv(gmdData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfGMD.csv', quote=F, row.names=F)

ctData <- merge(t1QA, jlfCt, by=c('bblid','scanid'))
#as ever write the csv...
write.csv(ctData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCt.csv', quote=F, row.names=F)

#Now do CC vals
ccData <- merge(t1QA, jlfCC, by=c('bblid', 'scanid'))
write.csv(ccData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCc.csv', quote=F, row.names=F)


# Now lets do all of the cbf data 
cbfData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfAntsCTIntersectionPcaslValues_20170403.csv')
cbfData[cbfData<20] <- NA
cbfWmData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfWMPcasl_20170407.csv')
cbfWmData[cbfWmData<5] <- NA
cbfQA <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_PcaslQaData_20170403.csv')

# Now merge the cbf data 
cbfData <- merge(cbfData, cbfWmData, by=c('bblid', 'scanid'))
cbfData <- merge(cbfQA, cbfData, by=c('bblid', 'scanid'))

# Write our csv
write.csv(cbfData , '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf-nonImpute.csv', quote=F, row.names=F)

# Now produce an imputed data frame
cbfData <- cbfData[which(cbfData$pcaslExclude==0),]
toImpute <- cbfData[,grep('jlf', names(cbfData))]
options(mc.cores=4)
toImpute <- mi(toImpute, parallel=T, seed=16)
output <- complete(toImpute, 4)
output.1 <- output[[1]][,1:127]
output.2 <- output[[2]][,1:127]
output.3 <- output[[3]][,1:127]
output.4 <- output[[4]][,1:127]
output <- (output.1 + output.2 + output.3 + output.4)/4
cbfDataImpute <- cbfData
cbfDataImpute[,grep('jlf', names(cbfData))] <- output

# Now write the imputed csv
write.csv(cbfDataImpute , '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf-Impute.csv', quote=F, row.names=F)

# Now do the resting data down here
alffData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfAntsCTIntersectionAlff_20170412.csv')
rehoData <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfAntsCTIntersectionReHo_20170412.csv')
restQa <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_RestQAData_20170318.csv')

# Now produce the reho values down here
alffData <- merge(restQa, alffData, by=c('bblid', 'scanid'))

# Now write the alff csv
write.csv(alffData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfAlff.csv', quote=F, row.names=F)

# Now do reho
rehoData <- merge(restQa, rehoData, by=c('bblid', 'scanid')) 

# writelasdfoshdfouhsdf
write.csv(rehoData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfReho.csv', quote=F, row.names=F)

# Now onto diffusion
# Start with all of the labels
adLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHULabelsAD_20170321.csv')
rdLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHULabelsRD_20170321.csv')
trLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHULabelsTR_20170321.csv')
faLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHULabelsFA_20170321.csv')
dtiQA <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw/dti/n1601_dti_QA_09_27_2016.csv')
dtiQA$bblid <- strSplitMatrixReturn(dtiQA$bblid, '_')[,1]

# Now merge our data and QA values
adData <- merge(adLabels, dtiQA, by=c('bblid', 'scanid'))
faData <- merge(faLabels, dtiQA, by=c('bblid', 'scanid'))
rdData <- merge(rdLabels, dtiQA, by=c('bblid', 'scanid'))
trData <- merge(trLabels, dtiQA, by=c('bblid', 'scanid'))

# sdfklhasfluhrfkljhaf write sd.kfjhasfuhasdflkhu
write.csv(adData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuADLabels.csv', quote=F, row.names=F)
write.csv(faData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuFALabels.csv', quote=F, row.names=F)
write.csv(rdData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuRDLabels.csv', quote=F, row.names=F)
write.csv(trData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuTRLabels.csv', quote=F, row.names=F)

# Now lets do tracts
adLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHUTractAD_20170321.csv')
rdLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHUTractRD_20170321.csv')
trLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHUTractTR_20170321.csv')
faLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_JHUTractFA_20170321.csv')

# Now merge with QA data
# Now merge our data and QA values
adData <- merge(adLabels, dtiQA, by=c('bblid', 'scanid'))
faData <- merge(faLabels, dtiQA, by=c('bblid', 'scanid'))
rdData <- merge(rdLabels, dtiQA, by=c('bblid', 'scanid'))
trData <- merge(trLabels, dtiQA, by=c('bblid', 'scanid'))

# Now write the csv
write.csv(adData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuADTracts.csv', quote=F, row.names=F)
write.csv(faData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuFATracts.csv', quote=F, row.names=F)
write.csv(rdData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuRDTracts.csv', quote=F, row.names=F)
write.csv(trData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jhuTRTracts.csv', quote=F, row.names=F)

# Now onto DTI GM values
adLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfADValues_20170411.csv')
rdLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfRDValues_20170411.csv')
trLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfTRValues_20170411.csv')
faLabels <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfFAValues_20170411.csv')
adLabelWM <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfWmLobesADValues_20170405.csv')
rdLabelWM <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfWmLobesRDValues_20170405.csv')
trLabelWM <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfWmLobesTRValues_20170405.csv')
faLabelWM <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_jlfWmLobesFAValues_20170405.csv')

# Merge WM values to the GM values
adLabels <- merge(adLabels, adLabelWM, by=intersect(names(adLabels), names(adLabelWM)))
rdLabels <- merge(rdLabels, rdLabelWM, by=intersect(names(rdLabels), names(rdLabelWM)))
trLabels <- merge(trLabels, trLabelWM, by=intersect(names(trLabels), names(trLabelWM)))
faLabels <- merge(faLabels, faLabelWM, by=intersect(names(faLabels), names(faLabelWM)))

# Now merge with QA data
# Now merge our data and QA values
adData <- merge(adLabels, dtiQA, by=c('bblid', 'scanid'))
faData <- merge(faLabels, dtiQA, by=c('bblid', 'scanid'))
rdData <- merge(rdLabels, dtiQA, by=c('bblid', 'scanid'))
trData <- merge(trLabels, dtiQA, by=c('bblid', 'scanid'))

#sdfkluhasfgliuhafrliuhasf csv
write.csv(adData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfAD.csv', quote=F, row.names=F)
write.csv(faData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfFA.csv', quote=F, row.names=F)
write.csv(rdData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfRD.csv', quote=F, row.names=F)
write.csv(trData, '/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfTR.csv', quote=F, row.names=F)

