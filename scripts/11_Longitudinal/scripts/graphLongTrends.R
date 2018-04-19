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
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-04.csv')
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
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-02.csv')
n1601.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
all.data$goassessDxpmr7 <- factor(all.data$goassessDxpmr7)
all.data$pncGrpPsych <- factor(all.data$pncGrpPsych)
all.data$Gender <- n1601.vals$sex[match(all.data$bblid, n1601.vals$bblid)]
true.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n705_oracle_sips_final_longitudinal_20170707.csv')
all.data <- all.data[all.data$bblid %in% true.data$bblid,]
## Now lets make our plots
summaryMetrics <- c('mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM', 'mprage_jlf_ct_MeanCT', 'mprage_jlf_gmd_MeanGMD', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF','F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis')
pdf('ageXAxisWithpncGrpPsychosisCl.pdf', height=20, width=28)
options(digits=7)
minVal <- c(800000, 500000, 300000, 2.5, .7,25, .05, 200, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -1)
maxVal <- c(1610000,1000000,700000,4.5,.95,110,.3,1000,2,2,2,2,2,2,2,2,2,2,7.5)
index <- 1
for(i in summaryMetrics){
  colVal <- grep(i, names(all.data))
  if(length(colVal)>1){
    colVal <- colVal[7]
  }
  toPlot <- all.data[complete.cases(all.data[,colVal]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsych==""),]
  #toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>2))),]
  toPlot$pncGrpPsych <- factor(toPlot$pncGrpPsych)
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4, fill=goassessDxpmr7)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsych, alpha=.1)) +
    geom_smooth(data=subset(toPlot, sex=="1"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, sex=="1")[,colVal], group=pncGrpPsych, col=pncGrpPsych)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Male") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4, fill=goassessDxpmr7)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsych, alpha=.1)) +
    geom_smooth(data=subset(toPlot,  sex=="2"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, sex=="2")[,colVal], group=pncGrpPsych, col=pncGrpPsych)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Female") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
  multiplot(pasta.plot.one,pasta.plot.two, cols=2)
  index <- index + 1
}
dev.off()

## Now lets make our plots w/o any labels
pdf('ageXAxisPersistVsAllElse.pdf', height=20, width=28)
index <- 1
for(i in summaryMetrics){
    colVal <- grep(i, names(all.data))
    toPlot <- all.data[complete.cases(all.data[,colVal]),]
    toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==""),]
    toPlot$pncGrpPsychosisCl <- factor(toPlot$pncGrpPsychosisCl)
    pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(data=subset(toPlot,pncGrpPsychosisCl=="TD" & sex=="1"),method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="TD" & sex=="1")[,colVal])) +
    geom_smooth(data=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="1"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="1")[,colVal], group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Male") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(data=subset(toPlot,pncGrpPsychosisCl=="TD" & sex=="2"),method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="TD" & sex=="2")[,colVal])) +
    geom_smooth(data=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="2"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="2")[,colVal], group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Female") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    multiplot(pasta.plot.one,pasta.plot.two, cols=2)
    index <- index + 1
}
dev.off()

## Now plot only those with multiple timepoints
pdf('ageXAxisPersistVsAllElseOnlyMTTTP.pdf', height=20, width=28)
index <- 1
for(i in summaryMetrics){
    colVal <- grep(i, names(all.data))
    colVal <- grep(i, names(all.data))
    if(length(colVal)>1){
      colVal <- colVal[7]
    }
    toPlot <- all.data[complete.cases(all.data[,colVal]),]
    toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==""),]
    toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl=="Emergent" | toPlot$pncGrpPsychosisCl=="Flux" | toPlot$pncGrpPsychosisCl=="Resilient"),]
    toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
    toPlot$pncGrpPsychosisCl <- factor(toPlot$pncGrpPsychosisCl)
    pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(data=subset(toPlot,pncGrpPsychosisCl=="TD" & sex=="1"),method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="TD" & sex=="1")[,colVal])) +
    geom_smooth(data=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="1"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="1")[,colVal], group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Male") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    geom_smooth(data=subset(toPlot,pncGrpPsychosisCl=="TD" & sex=="2"),method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="TD" & sex=="2")[,colVal])) +
    geom_smooth(data=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="2"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsychosisCl=="Persister" & sex=="2")[,colVal], group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Female") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
    multiplot(pasta.plot.one,pasta.plot.two, cols=2)
    index <- index + 1
}
dev.off()

# Now plot persist vs TD
pdf('ageXAxisTDVsPersist.pdf', height=20, width=28)
index <- 1
for(i in summaryMetrics){
  colVal <- grep(i, names(all.data))
  if(length(colVal)>1){
    colVal <- colVal[7]
  }
  toPlot <- all.data[complete.cases(all.data[,colVal]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsych==""),]
  toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsych=="Emergent" | toPlot$pncGrpPsych=="Flux" | toPlot$pncGrpPsych=="Resilient"),]
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4, fill=goassessDxpmr7)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "1")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsych, alpha=.1)) +
    geom_smooth(data=subset(toPlot, pncGrpPsych=="Persister" & sex=="1"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsych=="Persister" & sex=="1")[,colVal], group=pncGrpPsych, col=pncGrpPsych)) +
    geom_smooth(data=subset(toPlot, pncGrpPsych=="TD" & sex=="1"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsych=="TD" & sex=="1")[,colVal], group=pncGrpPsych, col=pncGrpPsych)) +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Male") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point(aes(shape=factor(goassessDxpmr7), size=4, fill=goassessDxpmr7)) +
    geom_point(data = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2"), aes(x = scanageMonths/12, y = subset(toPlot, goassessDxpmr7 == "TD" & sex == "2")[, colVal],shape = goassessDxpmr7),size=3,  color='white') +
    geom_line(aes(group=bblid, col=pncGrpPsych, alpha=.1)) +
    geom_smooth(data=subset(toPlot, pncGrpPsych=="Persister" & sex=="2"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsych=="Persister" & sex=="2")[,colVal], group=pncGrpPsych, col=pncGrpPsych)) +
    geom_smooth(data=subset(toPlot, pncGrpPsych=="TD" & sex=="2"), method='gam',formula = y ~ s(x, k=4),aes(x=scanageMonths/12, y=subset(toPlot, pncGrpPsych=="TD" & sex=="2")[,colVal], group=pncGrpPsych, col=pncGrpPsych)) +
    ylab(i) +
    theme_bw() +
    ggtitle("Female") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index]))
  multiplot(pasta.plot.one,pasta.plot.two, cols=2)
  index <- index + 1
}
dev.off()

# Now make an age by subject plot
toPlot <- all.data[complete.cases(all.data$scanageMonths),]
toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
toPlot <- toPlot[-which(toPlot$pncGrpPsych==levels(factor(toPlot$pncGrpPsych))[1]),]
toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
toPlot <- toPlot[-which(toPlot$pncGrpPsych!="Persister"),]
makeLevels <- which(toPlot$timepoint==1)
makeLevels2 <- order(toPlot$scanageMonths[makeLevels])
makeLevels3 <- toPlot$bblid[makeLevels][makeLevels2] 
toPlot$bblid <- factor(toPlot$bblid, levels=makeLevels3)
toPlot <- toPlot[-which(is.na(toPlot$bblid)),]
outPlot <- ggplot(toPlot, aes(x=scanageMonths/12, y=bblid, col=factor(sex), group=bblid)) +
  geom_point() +
  geom_line() +
  theme(text = element_text(size=20))
  #facet_grid(pncGrpPsych~., scales="free", space="free_y")
pdf("allLongAGeVals.pdf", width=20, height=50)
print(outPlot)
dev.off()

## Now do it in a histogram form
outPlot <- ggplot(toPlot) +
  geom_histogram(data=toPlot, aes(x=t2.t1_duration_months), fill='blue', alpha=.3,bins=50) +
  geom_histogram(data=toPlot, aes(x=t3.t1_duration_months), fill='red', alpha=.3,bins=50) +
  theme(text = element_text(size=20)) +
  xlab("Time from T1 (months)")
  


