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
## Now grab the PRS scores down here
prs.scores <- read.csv('/home/tymoore/PRS_Adon.csv')
all.data$PRS <- prs.scores$PRS[match(all.data$bblid, prs.scores$bblid)]

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
ageBinVals <- as.data.frame(cbind(all.data$bblid, all.data$scanid, returnAgeGroup(all.data$scanageMonths)))
colnames(ageBinVals) <- c('bblid', 'scanid', 'ageBin')
all.data <- merge(all.data, ageBinVals)

# Now I need to see how our cognitive and clinical factor scores perform over time in the longitudinal cohort
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis', 'Depression', 'Mania', 'Overall_Psychopathology_SIMPLE', 'Depression_SIMPLE', 'Mania_SIMPLE')
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
      tmpDat[,columnValue] <- ((tmpDat[,columnValue] - tmpDatMin[,columnValue])/tmpDatMin[,columnValue])*100
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
    geom_smooth(method='gam',aes(group=pncGrpPsychosisCl, col=pncGrpPsychosisCl)) +
    ylab(i) +
    theme_bw() + 
     coord_cartesian(ylim=c(-100, 100))
  print(pasta.plot.one)
}
dev.off()

# Now I need to find some IRR values across the factor scores tp 1 vs tp 2 and tp 1 vs tp 3
# I am prob going to do this across all diagnosis labels and then within the TD and PERSIS groups
# We need to make sure we use the freeze at this point here. 
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis', 'Depression', 'Mania', 'Overall_Psychopathology_SIMPLE', 'Depression_SIMPLE', 'Mania_SIMPLE')
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
  iccValue1 <- round(icc(longValues[,colVals[1:2]], "twoway", "agreement")$value, digits=2)
  iccValue2 <- round(icc(longValues[,colVals[1:3]], "twoway", "agreement")$value, digits=2)
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
    iccValue1 <- round(icc(longValues[,colVals[1:2]], "twoway", "agreement")$value, digits=2)
    iccValue2 <- round(icc(longValues[,colVals[1:3]], "twoway", "agreement")$value, digits=2)
    outValues[index,1] <- iccValue1
    outValues[index,2] <- iccValue2
    index <- index + 1
  }
  outMatAll <- cbind(outMatAll, outValues)
}

# Now write this csv
write.csv(outMatAll, "outputAgreementValues.csv", quote=F, row.names=T)

## Now we need to compare the mean values vs tp
## In order to do this we need to perform age regression
## We are also going to expand our summary metrics to include the imaging summary values
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis', 'Depression', 'Mania', 'Overall_Psychopathology_SIMPLE', 'Depression_SIMPLE', 'Mania_SIMPLE')
all.data <- freeze
all.data$pncGrpPsych <- factor(all.data$pncGrpPsych, levels=c("Persister", "Resilient", "Emergent", "TD", "Flux", ""))
for(s in summaryMetrics){
  columnValue <- grep(s, names(all.data))
    if(length(columnValue)>1){
      columnValue <- grep(paste("^", s, "$", sep=''), names(all.data))
  }
  tmpMat <- returnMeanSDValues(all.data[,columnValue], all.data$ageBin)
  #all.data[,columnValue] <- applyMeanandSD(tmpMat, all.data[,columnValue], all.data$ageBin)
  index <- names(residuals(lm(all.data[,columnValue]~age+age2+age3, data=all.data)))
  all.data[index,columnValue] <- scale(residuals(lm(all.data[,columnValue]~age+age2+age3, data=all.data)))
}

## Now we need to plot the mean vs timepoint here - will also add summary mean trajectories 
## for each of our clinical labels
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis', 'Depression', 'Mania', 'Overall_Psychopathology_SIMPLE', 'Depression_SIMPLE', 'Mania_SIMPLE')
options(digits=7)
pdf("tpVsSummaryFactorScoresMetric.pdf", width=18, height=16)
for(s in summaryMetrics){
  columnValue <- grep(s, names(all.data))
  if(length(columnValue)>1){
    columnValue <- grep(paste("^", s, "$", sep=''), names(all.data))
  }
  toPlot <- all.data[complete.cases(all.data[,columnValue]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsych==""),]
  toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
  plotVals <- summarySE(toPlot, measurevar=s, groupvars=c('timepoint', 'pncGrpPsych'), na.rm=T)
  plotVals <- plotVals[-which(plotVals$pncGrpPsych=='Flux'),]
  tmpPlot <- ggplot(plotVals, aes(x=as.factor(timepoint), y=plotVals[,4], shape=factor(pncGrpPsych), col=factor(pncGrpPsych))) + 
    geom_point(size=5, position=position_dodge(width=0.5)) +
    geom_errorbar(aes(ymin=plotVals[,4]-ci, ymax=plotVals[,4]+ci), position=position_dodge(width=0.5)) +
    geom_line(aes(x=timepoint, y=plotVals[,4]), position=position_dodge(width=0.5)) +
    theme_bw() +
    ylab(paste(s)) +
    scale_x_discrete(limits = c("1","2","3"), expand=c(0,.5)) +
    coord_cartesian(ylim=c(-2, 2)) +
    xlab("Timepoint")
  print(tmpPlot)
}
## Now do the ERS and PRS values down here
summaryMetrics <- c('envSES', 'PRS')
for(s in summaryMetrics){
  columnValue <- grep(s, names(all.data))
  if(length(columnValue)>1){
    columnValue <- grep(paste("^", s, "$", sep=''), names(all.data))
  }
  toPlot <- all.data[complete.cases(all.data[,columnValue]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsych==""),]
  toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
  plotVals <- summarySE(toPlot, measurevar=s, groupvars=c('pncGrpPsych'), na.rm=T)
  plotVals <- plotVals[-which(plotVals$pncGrpPsych=='Flux'),]
  tmpPlot <- ggplot(plotVals, aes(y=plotVals[,3], x=factor(pncGrpPsych), col=factor(pncGrpPsych))) + 
    geom_point(size=5) + 
    geom_errorbar(aes(ymin=plotVals[,3]-ci, ymax=plotVals[,3]+ci),width = .2) +
    theme_bw() +
    ylab(paste(s))
  print(tmpPlot)
}
dev.off()

## Now do the imaging values. 
## We are going to have to age regress w/in sex because we are adding the global mean
## I think this z score method is very dumb bbut thats just me...
summaryMetrics <- c('mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM', 'mprage_jlf_ct_MeanCT', 'mprage_jlf_gmd_MeanGMD', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF')
all.data <- freeze
## Now extend digit value in options
options(digits=22)
for(s in summaryMetrics){
  columnValue <- grep(s, names(all.data))
    if(length(columnValue)>1){
      columnValue <- grep(paste("^", s, "$", sep=''), names(all.data))
  }
  tmpMat <- returnMeanSDValues(all.data[which(all.data$Gender==1),columnValue], all.data$ageBin[which(all.data$Gender==1)])
  #all.data[which(all.data$Gender==1),columnValue] <- applyMeanandSD(tmpMat, all.data[which(all.data$Gender==1),columnValue], all.data$ageBin[which(all.data$Gender==1)])
  index <- names(residuals(lm(all.data[,columnValue]~age+age2+age3, data=all.data)))
  all.data[index,columnValue] <- scale(residuals(lm(all.data[,columnValue]~age+age2+age3, data=all.data)))
  tmpMat <- returnMeanSDValues(all.data[which(all.data$Gender==2),columnValue], all.data$ageBin[which(all.data$Gender==2)])
  #all.data[which(all.data$Gender==2),columnValue] <- applyMeanandSD(tmpMat, all.data[which(all.data$Gender==2),columnValue], all.data$ageBin[which(all.data$Gender==2)])
  #index <- names(residuals(lm(all.data[which(all.data$sex==2),columnValue]~age+age2+age3, data=all.data[which(all.data$sex==2),])))
  #all.data[index,columnValue] <- scale(residuals(lm(all.data[which(all.data$sex==2),columnValue]~age+age2+age3, data=all.data[which(all.data$sex==2),])))
}
minVal <- c(1100000, 550000,375000, 2.5, .77,35, .1, 300)
maxVal <- c(1300000,850000,575000,4.5,.85,65,.2,650)
index <- 1
pdf("tpVsSummaryImagingScoresMetric.pdf", width=24, height=16)
for(s in summaryMetrics){
  columnValue <- grep(s, names(all.data))
  if(length(columnValue)>1){
    columnValue <- grep(paste("^", s, "$", sep=''), names(all.data))
  }
  toPlot <- all.data[complete.cases(all.data[,columnValue]),]
  toPlot <- toPlot[-which(toPlot$pncGrpPsych==""),]
  toPlot <- toPlot[which(toPlot$bblid %in% names(which(table(toPlot$bblid)>1))),]
  plotVals1 <- summarySE(toPlot[which(toPlot$Gender==1),], measurevar=s, groupvars=c('timepoint', 'pncGrpPsych'), na.rm=T)
  plotVals1 <- plotVals1[-which(plotVals1$pncGrpPsych=='Flux'),]
  tmpPlot1 <- ggplot(plotVals1, aes(x=as.factor(timepoint), y=plotVals1[,4], shape=factor(pncGrpPsych), col=factor(pncGrpPsych))) + 
    geom_point(size=5, position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(ymin=plotVals1[,4]-ci, ymax=plotVals1[,4]+ci), position=position_dodge(width=0.5)) +
    geom_line(aes(x=timepoint, y=plotVals1[,4]),position=position_dodge(width=0.5)) +
    theme_bw() +
    ylab(paste(s)) + 
    xlab("Timepoint") +
    ggtitle('Male') +
    theme(legend.position="bottom",text = element_text(size=20)) +
    coord_cartesian(ylim=c(-2, 2)) +
    scale_x_discrete(limits = c("1","2","3"), expand=c(0,.5))
  plotVals2 <- summarySE(toPlot[which(toPlot$Gender==2),], measurevar=s, groupvars=c('timepoint', 'pncGrpPsych'), na.rm=T)
  plotVals2 <- plotVals2[-which(plotVals2$pncGrpPsych=='Flux'),]
  tmpPlot2 <- ggplot(plotVals2, aes(x=as.factor(timepoint), y=plotVals2[,4], shape=factor(pncGrpPsych), col=factor(pncGrpPsych))) + 
    geom_point(size=5, position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(ymin=plotVals2[,4]-ci, ymax=plotVals2[,4]+ci),position=position_dodge(width=0.5)) +
    geom_line(aes(x=timepoint, y=plotVals2[,4]),position=position_dodge(width=0.5)) +
    theme_bw() +
    ylab(paste(s)) + 
    xlab("Timepoint") +
    ggtitle('Female') +
    theme(legend.position="bottom",text = element_text(size=20)) +
    coord_cartesian(ylim=c(-2, 2)) +
    scale_x_discrete(limits = c("1","2","3"), expand=c(0,.5))
  multiplot(tmpPlot1, tmpPlot2, cols=2)
  index <- index + 1
}
dev.off()
