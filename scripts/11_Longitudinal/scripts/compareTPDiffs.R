# AFGR March 2018
# These script is going to be used to compare imbalances between 
# the clinical, cognitive, and neuroimaging summary metrics
# before and after age regression

## Library(s)
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'utils', 'irr')
source("~/hiLo/scripts/11_Longitudinal/functions/functions.R")

## Now load the data
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-02.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
all.data$goassessDxpmr7 <- factor(all.data$goassessDxpmr7)
all.data$pncGrpPsychosisCl <- factor(all.data$pncGrpPsychosisCl)
true.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n705_oracle_sips_final_longitudinal_20170707.csv')
all.data <- all.data[all.data$bblid %in% true.data$bblid,]
n1601.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')
all.data$Gender <- n1601.vals$sex[match(all.data$bblid, n1601.vals$bblid)]
## The first thing I need to do is find the time from T1 in days
all.data$timeDiff <- 0
# Now loop through each BBLID and find the time since time 1
output.vol <- NULL
for(i in unique(all.data$bblid)){
  ## Now add days from Scan 1
  tmpDat <- all.data[which(all.data$bblid==i),]
  print(dim(tmpDat))
  print(i)
  if(dim(tmpDat)[1]>1){
    minValue <- min(tmpDat$DOSCAN)
    tmpDat$timeDiff <- difftime(tmpDat$DOSCAN, minValue, units=c("day"))
  }
  # Now combine everything
  output.vol <- rbind(output.vol, tmpDat)
}
all.data <- output.vol
rm(output.vol)
all.data$age <- scale(all.data$scanageMonths)
all.data$age2 <- scale(all.data$age)^2
all.data$age3 <- scale(all.data$age)^3
ageBinVals <- as.data.frame(cbind(n1601.vals$bblid, returnAgeGroup(n1601.vals$ageAtScan1)))
colnames(ageBinVals) <- c('bblid', 'ageBin')
all.data <- merge(all.data, ageBinVals)

# Now I need to see how our cognitive and clinical factor scores perform over time in the longitudinal cohort
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis', 'Depression', 'Mania', 'Overall_Psychopathology_SIMPLE')
# Now create our age regeressed values
freeze <- all.data
to.use <- all.data
for(s in summaryMetrics){
  percent.diff.male <- NULL
  columnValue <- grep(s, names(all.data))
  if(length(columnValue)>1){
      columnValue <- grep(paste("^", s, "$", sep=''), names(all.data))
  }
  tmpMat <- returnMeanSDValues(to.use[,columnValue], to.use$ageBin)
  to.use[,columnValue] <- applyMeanandSD(tmpMat, to.use[,columnValue], all.data$ageBin)
  to.use[,columnValue] <- range12(to.use[,columnValue])
  pb <- txtProgressBar(min=0, max=length(unique(to.use$bblid)), initial=0, style=3)
  pbL <- 0
  for(i in unique(to.use$bblid)){
    ## Now find % difference 
    tmpDat <- to.use[to.use$bblid==i,]
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
  to.use[,columnValue] <- percent.diff.male[,columnValue]
}


# Now plot these
allPlot <- to.use
pdf('cogAndClinicalFactorScoresPercentDiff.pdf')
for(i in summaryMetrics){
  colVal <- grep(i, names(to.use))
  if(length(colVal)>1){
      colVal <- grep(paste("^", i, "$", sep=''), names(all.data))
  }
  toPlot <- to.use[complete.cases(to.use[,colVal]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==""),]
  toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl=="Emergent" | toPlot$pncGrpPsychosisCl=="Flux" | toPlot$pncGrpPsychosisCl=="Resilient"),]
  #toPlot <- toPlot[-which(toPlot$pncGrpPsychosisCl==levels(factor(toPlot$pncGrpPsychosisCl))[1]),]
  pasta.plot.one <- ggplot(toPlot, aes(x=timeDiff, y=toPlot[,colVal])) + 
    geom_point() + 
    geom_line(aes(group=bblid, col=pncGrpPsychosisCl, alpha=.1)) +
    #geom_smooth(method='gam',aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw()
  print(pasta.plot.one)
}
dev.off()

# Now I need to find some IRR values across the factor scores tp 1 vs tp 2 and tp 1 vs tp 3
# I am prob going to do this across all diagnosis labels and then within the TD and PERSIS groups
# We need to make sure we use the freeze at this point here. 
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis', 'Depression', 'Mania', 'Overall_Psychopathology_SIMPLE')
# Now prepare an output matrix with these values
outValues <- matrix(NA, nrow=length(summaryMetrics), ncol=2)
rownames(outValues) <- summaryMetrics
colnames(outValues) <- c('All.1.2', 'All.1.3')
# Now prepare the data in long format
tmpDat <- freeze[,c('bblid','timepoint', summaryMetrics)]
longValues <- reshape(tmpDat, timevar='timepoint', idvar='bblid', direction='wide')
index <- 1
for(i in summaryMetrics){
  colVals <- grep(i, colnames(longValues))
  iccValue1 <- icc(longValues[,colVals[1:2]], "twoway", "agreement")$value
  iccValue2 <- icc(longValues[,colVals[1:3]], "twoway", "agreement")$value
  outValues[index,1] <- iccValue1
  outValues[index,2] <- iccValue2
  index <- index + 1
}

## Now do the same within our clinical labels
outMatAll <- outValues
for(x in names(table(freeze$pncGrpPsychosisCl))[-1]){
  outValues <- matrix(NA, nrow=length(summaryMetrics), ncol=2)
  rownames(outValues) <- summaryMetrics
  colnames(outValues) <- c(paste(x, '.1.2', sep=''),paste(x, '.1.3', sep='')) 
  # Now prepare the data in long format
  tmpDat <- freeze[which(freeze$pncGrpPsychosisCl==x),c('bblid','timepoint', summaryMetrics)]
  longValues <- reshape(tmpDat, timevar='timepoint', idvar='bblid', direction='wide')
  index <- 1
  for(i in summaryMetrics){
    colVals <- grep(i, colnames(longValues))
    iccValue1 <- icc(longValues[,colVals[1:2]], "twoway", "agreement")$value
    iccValue2 <- icc(longValues[,colVals[1:3]], "twoway", "agreement")$value
    outValues[index,1] <- iccValue1
    outValues[index,2] <- iccValue2
    index <- index + 1
  }
  outMatAll <- cbind(outMatAll, outValues)
}

# Now write this csv
write.csv(outMatAll, "outputAgreementValues.csv", quote=F, row.names=T)
