## Library(s)
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'utils')
source("~/hiLo/scripts/11_Longitudinal/functions/functions.R")

## Now extend digit value in options
options(digits=22)

## Data
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/volumeData.csv')
vol.data$DOSCAN <- as.character(vol.data$DOSCAN)
vol.data$DOSCAN <- as.Date(vol.data$DOSCAN, "%m/%d/%y")
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/alffData.csv')
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-03-20.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
n1601.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')
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
all.data <- merge(all.data, timeDiffVals)
all.data$age <- scale(all.data$scanageMonths)
all.data$age2 <- scale(all.data$age)^2
all.data$age3 <- scale(all.data$age)^3

# Now produce the mean and sd values for errybody
ageBinVals <- as.data.frame(cbind(n1601.vals$bblid, returnAgeGroup(n1601.vals$ageAtScan1)))
colnames(ageBinVals) <- c('bblid', 'ageBin')
all.data <- merge(all.data, ageBinVals, all=T)

## Now produce the age regressed by z scoring values
## This should be done within gender
summaryMetrics <- c('mprage_jlf_vol_TBV', 'mprage_jlf_ct_MeanCT', 'mprage_jlf_gmd_MeanGMD', 'pcasl_jlf_cbf_MeanGMCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF')
all.data.male <- all.data[which(all.data$sex==1),]
percent.diff.male.out <- all.data.male
for(s in summaryMetrics){
  percent.diff.male <- NULL
  columnValue <- grep(s, names(all.data))
  tmpMat <- returnMeanSDValues(all.data.male[,columnValue], all.data.male$ageBin)
  all.data.male[,columnValue] <- applyMeanandSD(tmpMat, all.data.male[,columnValue], all.data.male$ageBin)
  pb <- txtProgressBar(min=0, max=length(unique(all.data.male$bblid)), initial=0, style=3)
  pbL <- 0
  for(i in unique(all.data.male$bblid)){
    ## Now find % difference 
    tmpDat <- all.data.male[all.data.male$bblid==i,]
    if(dim(tmpDat)[1]>1){
      # Now if we have multiple time points find the minimum time, and compare everything to that guy
      tmpDatMin <- tmpDat[which(tmpDat$DOSCAN==min(tmpDat$DOSCAN)),]
      tmpDat[,columnValue] <- (tmpDat[,columnValue] - tmpDatMin[,columnValue])/tmpDatMin[,columnValue]
    }
    else{
      tmpDat[,columnValue] <- 0
    }
    # Now combine everything
    setTxtProgressBar(pb, pbL)
    pbL <- pbL + 1
    percent.diff.male <- rbind(percent.diff.male, tmpDat)
  }
  percent.diff.male.out[,columnValue] <- percent.diff.male[,columnValue]
}
## Now quickly apply some QA
percent.diff.male.out$mprage_jlf_gmd_MeanGMD[which(percent.diff.male.out$mprage_jlf_gmd_MeanGMD< -10)] <- NA
percent.diff.male.out$mprage_jlf_gmd_MeanGMD[which(percent.diff.male.out$mprage_jlf_gmd_MeanGMD> 10)] <- NA

## Now do the same for females
all.data.female <- all.data[which(all.data$sex==2),]
percent.diff.female.out <- all.data.female
for(s in summaryMetrics){
  percent.diff.female <- NULL
  columnValue <- grep(s, names(all.data))
  tmpMat <- returnMeanSDValues(all.data.female[,columnValue], all.data.female$ageBin)
  all.data.female[,columnValue] <- applyMeanandSD(tmpMat, all.data.female[,columnValue], all.data.female$ageBin)
  pb <- txtProgressBar(min=0, max=length(unique(all.data.female$bblid)), initial=0, style=3)
  pbL <- 0
  for(i in unique(all.data.female$bblid)){
    ## Now find % difference 
    tmpDat <- all.data.female[all.data.female$bblid==i,]
    if(dim(tmpDat)[1]>1){
      # Now if we have multiple time points find the minimum time, and compare everything to that guy
      tmpDatMin <- tmpDat[which(tmpDat$DOSCAN==min(tmpDat$DOSCAN)),]
      tmpDat[,columnValue] <- (tmpDat[,columnValue] - tmpDatMin[,columnValue])/tmpDatMin[,columnValue]
    }
    else{
      tmpDat[,columnValue] <- 0
    }
    # Now combine everything
    setTxtProgressBar(pb, pbL)
    pbL <- pbL + 1
    percent.diff.female <- rbind(percent.diff.female, tmpDat)
  }
  percent.diff.female.out[,columnValue] <- percent.diff.female[,columnValue]
}
## Now do some QA
percent.diff.female.out$mprage_jlf_gmd_MeanGMD[which(percent.diff.female.out$mprage_jlf_gmd_MeanGMD< -10)] <- NA
percent.diff.female.out$mprage_jlf_gmd_MeanGMD[which(percent.diff.female.out$mprage_jlf_gmd_MeanGMD> 10)] <- NA
percent.diff.female.out$rest_jlf_reho_MeanReho[which(percent.diff.female.out$rest_jlf_reho_MeanReho< -10)] <- NA
percent.diff.female.out$rest_jlf_reho_MeanReho[which(percent.diff.female.out$rest_jlf_reho_MeanReho> 10)] <- NA

# Now isolate the data to those subjects we are only interested in
# First do the TD and emergent comparison
graph.vals.one <- percent.diff.male.out[which(percent.diff.male.out$pncGrpPsychosisCl=="TD" | percent.diff.male.out$pncGrpPsychosisCl=="Emergent"),]
graph.vals.two <- percent.diff.male.out[which(percent.diff.male.out$pncGrpPsychosisCl=="Persister" | percent.diff.male.out$pncGrpPsychosisCl=="Resilient"),]

options(digits=7)
pdf('maleRawValuesTvEAR.pdf')
for(i in summaryMetrics){
  colVal <- grep(i, names(graph.vals.one))
  toPlot <- graph.vals.one[complete.cases(graph.vals.one[,colVal]),]
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=timeDiff, y=toPlot[which(toPlot$sex==1),colVal])) + 
    geom_point() + 
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw()
  print(pasta.plot.one)
}
dev.off()

## pers vs res
pdf('maleRawValuesPvRAR.pdf')
for(i in summaryMetrics){
  colVal <- grep(i, names(graph.vals.one))
  toPlot <- graph.vals.two[complete.cases(graph.vals.two[,colVal]),]
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=timeDiff, y=toPlot[which(toPlot$sex==1),colVal])) + 
    geom_point() + 
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw()
  print(pasta.plot.one)
}
dev.off()

## Now do females
graph.vals.one <- percent.diff.female.out[which(percent.diff.female.out$pncGrpPsychosisCl=="TD" | percent.diff.female.out$pncGrpPsychosisCl=="Emergent"),]
graph.vals.two <- percent.diff.female.out[which(percent.diff.female.out$pncGrpPsychosisCl=="Persister" | percent.diff.female.out$pncGrpPsychosisCl=="Resilient"),]

pdf('femaleRawValuesTvEAR.pdf')
for(i in summaryMetrics){
  colVal <- grep(i, names(graph.vals.one))
  toPlot <- graph.vals.one[complete.cases(graph.vals.one[,colVal]),]
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=timeDiff, y=toPlot[which(toPlot$sex==2),colVal])) + 
    geom_point() + 
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw()
  print(pasta.plot.one)
}
dev.off()

pdf('femaleRawValuesPvRAR.pdf')
for(i in summaryMetrics){
  colVal <- grep(i, names(graph.vals.one))
  toPlot <- graph.vals.two[complete.cases(graph.vals.two[,colVal]),]
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=timeDiff, y=toPlot[which(toPlot$sex==2),colVal])) + 
    geom_point() + 
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw()
  print(pasta.plot.one)
}
dev.off()

## Now graph these values w/o and age regrssion, just mean trends across time 
## with the x axis being age in years, and the y axis being our imaging metrics
## Lets first reload our data
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-03-20.csv')
n1601.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
all.data$goassessDxpmr7 <- factor(all.data$goassessDxpmr7)
all.data$pncGrpPsychosisCl <- factor(all.data$pncGrpPsychosisCl)
all.data$Gender <- n1601.vals$sex[match(all.data$bblid, n1601.vals$bblid)]
## Now lets make our plots
summaryMetrics <- c('mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM', 'mprage_jlf_ct_MeanCT', 'mprage_jlf_gmd_MeanGMD', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF')
pdf('ageXAxisWithpncGrpPsychosisCl.pdf', height=23, width=20)
options(digits=7)
minVal <- c(800000, 500000, 300000, 2.5, .7,25, .05, 200)
maxVal <- c(1610000,1000000,700000,4.5,.95,110,.3,1000)
index <- 1
for(i in summaryMetrics){
  colVal <- grep(i, names(all.data))
  toPlot <- all.data[complete.cases(all.data[,colVal]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==""),]
  toPlot$pncGrpPsychosisCl <- factor(toPlot$pncGrpPsychosisCl)
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',formula = y ~ s(x, k=4),aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Male") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',formula = y ~ s(x, k=4),aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Female") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
  multiplot(pasta.plot.one,pasta.plot.two, cols=2)
  index <- index + 1
}
dev.off()

# Now do plots with timepoint specific labels
pdf('ageXAxisWithgoassess.pdf', height=23, width=20)
index <- 1
for(i in summaryMetrics){
    colVal <- grep(i, names(all.data))
    toPlot <- all.data[complete.cases(all.data[,colVal]),]
    toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==""),]
    toPlot$pncGrpPsychosisCl <- factor(toPlot$pncGrpPsychosisCl)
    pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_line(aes(group=bblid, alpha=.1)) +
    geom_smooth(method='gam',formula = y ~ s(x, k=4),aes(group=goassessDxpmr7, col=goassessDxpmr7)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Male") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_line(aes(group=bblid, alpha=.1)) +
    geom_smooth(method='gam',formula = y ~ s(x, k=4),aes(group=goassessDxpmr7, col=goassessDxpmr7)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Female") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    multiplot(pasta.plot.one,pasta.plot.two, cols=2)
    index <- index + 1
}
dev.off()

## Now lets make our plots w/o any labels
pdf('ageXAxis.pdf', height=23, width=20)
index <- 1
for(i in summaryMetrics){
    colVal <- grep(i, names(all.data))
    toPlot <- all.data[complete.cases(all.data[,colVal]),]
    toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==""),]
    toPlot$pncGrpPsychosisCl <- factor(toPlot$pncGrpPsychosisCl)
    pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',formula = y ~ s(x, k=4)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Male") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(method='gam',formula = y ~ s(x, k=4)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Female") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    multiplot(pasta.plot.one,pasta.plot.two, cols=2)
    index <- index + 1
}
dev.off()