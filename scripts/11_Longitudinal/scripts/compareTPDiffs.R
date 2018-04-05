# AFGR March 2018
# These script is going to be used to compare imbalances between 
# the clinical, cognitive, and neuroimaging summary metrics
# before and after age regression

## Library(s)
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'utils')
source("~/hiLo/scripts/11_Longitudinal/functions/functions.R")

## Now load the data
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-02.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
all.data$goassessDxpmr7 <- factor(all.data$goassessDxpmr7)
all.data$pncGrpPsychosisCl <- factor(all.data$pncGrpPsychosisCl)
all.data$Gender <- n1601.vals$sex[match(all.data$bblid, n1601.vals$bblid)]
true.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/n705_oracle_sips_final_longitudinal_20170707.csv')
all.data <- all.data[all.data$bblid %in% true.data$bblid,]
n1601.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/n1601_demographics_go1_20161212.csv')
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
timeDiffVals <- output.vol[,c('bblid', 'scanid', 'timeDiff')]
all.data <- merge(all.data, timeDiffVals)
all.data$age <- scale(all.data$scanageMonths)
all.data$age2 <- scale(all.data$age)^2
all.data$age3 <- scale(all.data$age)^3
ageBinVals <- as.data.frame(cbind(n1601.vals$bblid, returnAgeGroup(n1601.vals$ageAtScan1)))
colnames(ageBinVals) <- c('bblid', 'ageBin')
all.data <- merge(all.data, ageBinVals, all=T)

# Now I nneed to see how our cognitive and clinical factor scores perform over time in the longitudinal cohort
summaryMetrics <- c('F1_Exec_Comp_Cog_Accuracy', 'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy', 'F1_Slow_Speed', 'F2_Memory_Speed', 'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency', 'F3_Memory_Efficiency', 'F4_Executive_Efficiency', 'Psychosis')
# Now create our age regeressed values
for(s in summaryMetrics){
  percent.diff.male <- NULL
  columnValue <- grep(s, names(all.data))
  if(length(columnValue)>1){
      columnValue <- columnValue[7]
  }
  tmpMat <- returnMeanSDValues(all.data[,columnValue], all.data$ageBin)
  all.data[,columnValue] <- applyMeanandSD(tmpMat, all.data[,columnValue], all.data$ageBin)
  pb <- txtProgressBar(min=0, max=length(unique(all.data$bblid)), initial=0, style=3)
  pbL <- 0
  for(i in unique(all.data$bblid)){
    ## Now find % difference 
    tmpDat <- all.data[all.data$bblid==i,]
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
