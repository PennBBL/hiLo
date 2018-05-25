## AFGR this script is goinng to be used to compare several different normaztive growth curves
## using various types of data. Specificially it will build a cross sectional growth curve tfor the td subjects
## a long model with mixed effects, and finally a mem with subjects w/ 1 tp and more
## This will be done in all subbjects with a terminal TD timepoint
## ALso I am only going to do this for the male cohorts

## Load library(s)
install_load('lme4', 'mgcv', 'ggplot2', 'gamm4', 'voxel')

## Load the data
all.data <- read.csv('/home/adrose/forRuben/data/tmp/n2416_imagingDataDump_2018-05-15.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
all.data.go1 <- read.csv('/home/adrose/forRuben/data/n1601_imagingDataDump_2018-05-04.csv')
dx.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n1601_diagnosis_dxpmr_20170509.csv')
all.data.go1 <- merge(all.data.go1, dx.data)
all.data <- merge(all.data, dx.data, by='bblid')

# Now declare a td vs !td variable factor for go1
all.data.go1$notPS <- 'NONPS'
all.data.go1$notPS[all.data.go1$goassessDxpmr7=='PS'] <- 'PS'
all.data.go1$notPS <- factor(all.data.go1$notPS)

# Build a cross-sectional growth trajectory for the TD group from Go1
cs.mod <- gam(mprage_jlf_vol_TBWM ~ s(ageAtScan1, by=notPS) + notPS + averageManualRating, data=all.data.go1[which(all.data.go1$sex==1),], REML=T)
cs.mod.1 <- gam(mprage_jlf_vol_TBGM ~ s(ageAtScan1, by=notPS) + notPS + averageManualRating, data=all.data.go1[which(all.data.go1$sex==1),], REML=T)

## Now I need to make a TD vs !TD terminal diagnosis variable 
all.data$terminal <- 'NONPS'
output.vol <- NULL
for(i in unique(all.data$bblid)){
  ## Now add days from Scan 1
  tmpDat <- all.data[which(all.data$bblid==i),]
  if(dim(tmpDat)[1]>1){
    maxValue <- max(tmpDat$DOSCAN)
    # Now grab the tp value for the max value
    maxTpVal <- tmpDat$tpvalue[which(tmpDat$DOSCAN==maxValue)]
    colname <- paste('dx_t', maxTpVal, '_psychosis', sep='')
    toUse <- as.character(tmpDat[,colname])
    toUse <- unique(toUse[toUse != ""])
    if(identical(toUse, character(0))){
      toUse <- as.character(tmpDat[,'goassessDxpmr7.y'])
      toUse <- unique(toUse[toUse != ""])
    }
    print(c(i, toUse))
    tmpDat$terminal <- toUse
  }
  else{
    toUse <- as.character(dx.data[which(dx.data$bblid==i),'goassessDxpmr7'])
    print(c(i, toUse))
    if(!identical(toUse, character(0))){
      tmpDat$terminal <- toUse
    }
  }
  # Now combine everything
  if(!identical(toUse, character(0))){
    output.vol <- rbind(output.vol, tmpDat)
  }
}
all.data.mem <- output.vol
rm(output.vol)
rm(tmpDat)

## Now create an initial variable
all.data.mem$initial <- 'NONPS'
output.vol <- NULL
for(i in unique(all.data.mem$bblid)){
  ## Now add days from Scan 1
  tmpDat <- all.data.mem[which(all.data.mem$bblid==i),]
  if(dim(tmpDat)[1]>1){
    minValue <- min(tmpDat$DOSCAN)
    # Now grab the tp value for the max value
    minTpVal <- tmpDat$tpvalue[which(tmpDat$DOSCAN==minValue)]
    colname <- paste('dx_t', minTpVal, '_psychosis', sep='')
    toUse <- as.character(tmpDat[,colname])
    toUse <- unique(toUse[toUse != ""])
    if(identical(toUse, character(0))){
      toUse <- as.character(tmpDat[,'goassessDxpmr7.y'])
      toUse <- unique(toUse[toUse != ""])
    }
    print(c(i, toUse))
    tmpDat$initial <- toUse
  }
  else{
    toUse <- as.character(dx.data[which(dx.data$bblid==i),'goassessDxpmr7'])
    print(c(i, toUse))
    if(!identical(toUse, character(0))){
      tmpDat$initial <- toUse
    }
  }
  # Now combine everything
  if(!identical(toUse, character(0))){
    output.vol <- rbind(output.vol, tmpDat)
  }
}
all.data.mem <- output.vol
rm(output.vol)
rm(tmpDat)

## Quickly fix our terminal variable
all.data.mem$terminal[which(all.data.mem$terminal!="PS")] <- "NONPS"
all.data.mem$terminal <- factor(all.data.mem$terminal)
all.data.mem$initial[which(all.data.mem$initial!="PS")] <- "NONPS"
all.data.mem$initial <- factor(all.data.mem$initial)

## Now declare a binary multiple vs not multiple tp variable
all.data.mem$multTP <- 0 
all.data.mem$multTP[which(all.data.mem$bblid %in%  names(which(table(all.data.mem$bblid)>1)))] <- 1
all.data.mem$multTP <- factor(all.data.mem$multTP)

# Now build a long gamm model
# First turn all of our missing n timepoint values to 1
index <- names(which(table(all.data.mem$bblid)>1))
mem.data <- all.data.mem[which(all.data.mem$bblid %in% index),]
mem.mod <- gamm4::gamm4(mprage_jlf_vol_TBWM ~ s(scanageMonths, by=terminal)+ terminal + averageManualRating,random=~(1|bblid), data=all.data.mem[which(all.data.mem$sex==1),], REML=T)
mem.mod.1 <- gamm4::gamm4(mprage_jlf_vol_TBGM ~ s(scanageMonths, by=terminal)+ terminal + averageManualRating,random=~(1|bblid), data=all.data.mem[which(all.data.mem$sex==1),], REML=T)
mem.mod.2 <- gamm4::gamm4(mprage_jlf_vol_TBWM ~ s(scanageMonths, by=initial) + initial + averageManualRating, random=~(1|bblid), data=all.data.mem[which(all.data.mem$sex==1),], REML=T)
mem.mod.long <- gamm4(mprage_jlf_vol_TBWM ~ s(scanageMonths, by=terminal)+terminal + averageManualRating,random=~(1|bblid), data=mem.data[which(mem.data$sex==1),], REML=T)
mem.mod.long.1 <- gamm4(mprage_jlf_vol_TBGM ~ s(scanageMonths, by=terminal)+terminal + averageManualRating,random=~(1|bblid), data=mem.data[which(mem.data$sex==1),], REML=T)
mem.mod.long.2 <- gamm4(mprage_jlf_vol_TBWM ~ s(scanageMonths, by=initial)+initial + averageManualRating,random=~(1|bblid), data=mem.data[which(mem.data$sex==1),], REML=T)


## Now plot our three models
plot.cs <- plotGAM(gamFit=cs.mod, smooth.cov='ageAtScan1', rawOrFitted='raw', groupCovs='notPS')
df <- data.frame()
space.fill <- ggplot(df) + geom_blank() + theme_bw()
plot.mem.long <- plotGAMM(gammFit=mem.mod.long, smooth.cov='scanageMonths',rawOrFitted='raw',grouping='bblid',groupCovs='terminal')
plot.mem.long.1 <- plotGAMM(gammFit=mem.mod.long, smooth.cov='scanageMonths',rawOrFitted='raw',groupCovs='terminal')
plot.mem <- plotGAMM(gammFit=mem.mod, smooth.cov='scanageMonths',rawOrFitted='raw',grouping='bblid',groupCovs='terminal')
plot.mem.1 <- plotGAMM(gammFit=mem.mod, smooth.cov='scanageMonths',rawOrFitted='raw',groupCovs='terminal')
plot.mem.2 <- plotGAMM(gammFit=mem.mod.2, smooth.cov='scanageMonths', rawOrFitted='raw', grouping='bblid',groupCovs='initial')
plot.mem.3 <- plotGAMM(gammFit=mem.mod.2, smooth.cov='scanageMonths', rawOrFitted='raw',groupCovs='initial')
## Now standardize our models
plot.cs <- plot.cs + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("Cross Sectional Go1") + scale_colour_manual(name = "notPS",values=c("PS"="red","NONPS"="blue"))
plot.mem <- plot.mem + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("All data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
plot.mem.1 <- plot.mem.1 + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("All data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
plot.mem.2 <- plot.mem.2 + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("All data")+scale_colour_manual(name = "initial",values=c("PS"="red","NONPS"="blue"))
plot.mem.3 <- plot.mem.3 + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("All data")+scale_colour_manual(name = "initial",values=c("PS"="red","NONPS"="blue"))
plot.mem.long <- plot.mem.long + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("Only long data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
plot.mem.long.1 <- plot.mem.long.1 + coord_cartesian(ylim=c(300000,700000),xlim=c(90,275)) + ggtitle("Only long data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
pdf("comparingGrowthCurvesWM.pdf", height=18, width=28)
multiplot(plot.cs, space.fill, plot.mem.long, plot.mem.long.1, plot.mem, plot.mem.1, cols=3)
dev.off()

pdf("initialVsTerminal.pdf", height=18, width=24)
multiplot(plot.mem, plot.mem.1, plot.mem.2, plot.mem.3, cols=2)
dev.off()

# Now do the same for our GM
plot.cs <- plotGAM(gamFit=cs.mod.1, smooth.cov='ageAtScan1', rawOrFitted='raw', groupCovs='notPS')
df <- data.frame()
space.fill <- ggplot(df) + geom_blank() + theme_bw()
plot.mem.long <- plotGAMM(gammFit=mem.mod.long.1, smooth.cov='scanageMonths',rawOrFitted='raw',grouping='bblid',groupCovs='terminal')
plot.mem.long.1 <- plotGAMM(gammFit=mem.mod.long.1, smooth.cov='scanageMonths',rawOrFitted='raw',groupCovs='terminal')
plot.mem <- plotGAMM(gammFit=mem.mod.1, smooth.cov='scanageMonths',rawOrFitted='raw',grouping='bblid',groupCovs='terminal')
plot.mem.1 <- plotGAMM(gammFit=mem.mod.1, smooth.cov='scanageMonths',rawOrFitted='raw',groupCovs='terminal')
## Now standardize our models
plot.cs <- plot.cs + coord_cartesian(ylim=c(500000,1000000),xlim=c(90,275)) + ggtitle("Cross Sectional Go1") +scale_colour_manual(name = "notPS",values=c("PS"="red","NONPS"="blue"))
plot.mem <- plot.mem + coord_cartesian(ylim=c(500000,1000000),xlim=c(90,275)) + ggtitle("All data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
plot.mem.1 <- plot.mem.1 + coord_cartesian(ylim=c(500000,1000000),xlim=c(90,275)) + ggtitle("All data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
plot.mem.long <- plot.mem.long + coord_cartesian(ylim=c(500000,1000000),xlim=c(90,275)) + ggtitle("Only long data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
plot.mem.long.1 <- plot.mem.long.1 + coord_cartesian(ylim=c(500000,1000000),xlim=c(90,275)) + ggtitle("Only long data")+scale_colour_manual(name = "terminal",values=c("PS"="red","NONPS"="blue"))
pdf("comparingGrowthCurvesGM.pdf", height=18, width=28)
multiplot(plot.cs, space.fill, plot.mem.long, plot.mem.long.1, plot.mem, plot.mem.1, cols=3)
dev.off()

## Now find the minimum and maximum predicted values across our three models w/in a specific age range
max.val <- max(all.data.go1[which(all.data.go1$sex==1 & all.data.go1$goassessDxpmr7!='PS'),'ageAtScan1'])
min.val <- min(29mem.data[which(mem.data$sex==1 & mem.data$terminal =='NONPS'),'scanageMonths'])

## Now find our indices
## I need to talk about this w/ Tyler some more - I'm not sure the best way to proceed with this
q()
cs.max.index <- which(all.data.go1[which(all.data.go1$sex==1 & all.data.go1$goassessDxpmr7!='PS'),'ageAtScan1']==max.val)
mem.max.index <- which(all.data.mem[which(all.data.mem$sex==1 & all.data.mem$terminal =='NONPS'),]==max.val)
mem.long.index <- which(all.)
