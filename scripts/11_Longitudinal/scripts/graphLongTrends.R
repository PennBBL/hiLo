## Library(s)
install_load('ggplot2')
## Data
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/volumeData.csv')
vol.data$DOSCAN <- as.character(vol.data$DOSCAN)
vol.data$DOSCAN <- as.Date(vol.data$DOSCAN, "%m/%d/%y")
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/alffData.csv')
## The first thing I need to do is find the time from T1 in days
vol.data$timeDiff <- 0
# Now loop through each BBLID and find the time since time 1
output.vol <- NULL
for(i in unique(vol.data$bblid)){
  ## Now add days from Scan 1
  tmpDat <- vol.data[which(vol.data$bblid==i),]
  print(dim(tmpDat))
  print(i)
  if(dim(tmpDat)[1]>1){
    minValue <- min(tmpDat$DOSCAN)
    tmpDat$timeDiff <- difftime(tmpDat$DOSCAN, minValue, units=c("day"))
  }
  # Now combine everything
  output.vol <- rbind(output.vol, tmpDat)
}
timeDiffVals <- output.vol[,c('bblid', 'scanid', 'timeDiff')]
cbf.data <- merge(cbf.data, timeDiffVals)
gmd.data <- merge(gmd.data, timeDiffVals)

# Now plot differences in TBV et cetra
pasta.plot.one <- ggplot(output.vol[which(output.vol$sex==1),], aes(x=timeDiff, y=mprage_jlf_vol_ICV, group=bblid)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  #geom_line() +
  geom_smooth(method='gam',formula=y~x, level=0)
pasta.plot.two <- ggplot(cbf.data[which(cbf.data$sex==1),], aes(x=timeDiff, y=pcaslMeanGMValue, group=bblid)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  #geom_line() +
  geom_smooth(method='gam',formula=y~x, level=0)
pasta.plot.three <- ggplot(gmd.data[which(gmd.data$sex==1),], aes(x=timeDiff, y=mprage_jlf_gmd_MeanGMD, group=bblid)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  #geom_line() +
  geom_smooth(method='gam',formula=y~x, level=0)


pdf('test.pdf', height=30, width=50)
pasta.plot.one
pasta.plot.two
pasta.plot.three
dev.off()
