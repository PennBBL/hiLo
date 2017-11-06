install_load('psych', 'sas7bdat', 'mi', 'methods', 'doParallel')

# Start with T1 values 
jlfVol <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/volValuesPropHead.csv')
icvVol <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/mgiICV.csv')
jlfWmVol <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/mgiWmVals.txt')
jlfCt <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/ctValuesPropHead.csv')
jlfCC <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/ccValuesPropHead.csv')
jlfGmd <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/gmdValuesPropHead.csv')
meanGmd <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/mgiGMD.csv')
t1QA <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/n920_manual_ratings_validation.csv')

# Now merge the T1 data
volData <- merge(t1QA, jlfVol, by=c('bblid'))
volData <- merge(volData, jlfWmVol, by=c('bblid', 'scanid'))
volData <- merge(volData, icvVol, by=c('bblid','scanid'))

# Now write the csv
write.csv(volData, '/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_antsCtVol_jlfVol.csv', quote=F, row.names=F)


# Now write the other csv's
# Now do the GMD and CT and cortcon
gmdData <- merge(t1QA, jlfGmd, by=c('bblid'))
gmdData <- merge(gmdData, meanGmd, by=c('bblid', 'scanid'))

#write csv...
write.csv(gmdData, '/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_jlfGMD.csv', quote=F, row.names=F)

ctData <- merge(t1QA, jlfCt, by=c('bblid'))
#as ever write the csv...
write.csv(ctData, '/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_jlfCt.csv', quote=F, row.names=F)

#Now do CC vals
ccData <- merge(t1QA, jlfCC, by=c('bblid'))
write.csv(ccData, '/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_jlfCc.csv', quote=F, row.names=F)
