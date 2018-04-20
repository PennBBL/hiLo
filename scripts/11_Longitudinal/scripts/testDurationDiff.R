# AFGR April 2018
# This scipt will be used to explore if there are any differences in duration from T1 assessment in 
# any of our long labels
# Its essientially just going to be a 5x1 Anova
# if something comes back significant I'll explore where the differences are
# hopefully that isn't a problem

## Load library(s)
install_load('psych', 'ggplot2')

## Load the data
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR2416/volumeData.csv')
vol.data$DOSCAN <- as.character(vol.data$DOSCAN)
vol.data$DOSCAN <- as.Date(vol.data$DOSCAN, "%m/%d/%y")
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-04.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")
## The first thing I need to do is find the time from T1 in days
vol.data$timeDiff <- 0
vol.data$ageT1 <- 0
# Now loop through each BBLID and find the time since time 1
output.vol <- NULL
for(i in unique(vol.data$bblid)){
  ## Now add days from Scan 1
  tmpDat <- vol.data[which(vol.data$bblid==i),]
  print(dim(tmpDat))
  print(i)
  if(dim(tmpDat)[1]>1){
    minValue <- min(tmpDat$DOSCAN)
    minAge <- min(tmpDat$scanageMonths)
    tmpDat$timeDiff <- difftime(tmpDat$DOSCAN, minValue, units=c("day"))
    tmpDat$ageT1 <- minAge
    
  }
  # Now combine everything
  output.vol <- rbind(output.vol, tmpDat)
}
timeDiffVals <- output.vol[,c('bblid', 'scanid', 'timeDiff', 'ageT1')]
all.data <- merge(all.data, timeDiffVals)
all.data$age <- scale(all.data$scanageMonths)
all.data$age2 <- scale(all.data$age)^2
all.data$age3 <- scale(all.data$age)^3

## Now we need to make the anova to test the Duration differences
all.data$ageT1 <- as.numeric(all.data$ageT1)
all.data$pncGrpPsych <- factor(all.data$pncGrpPsych)
tp.two.data <- all.data[which(all.data$timepoint==2),]
tp.two.data <- tp.two.data[-which(tp.two.data$pncGrpPsych==''),]
tp.two.data <- tp.two.data[-which(tp.two.data$pncGrpPsych=='Flux'),]

mod1 <- aov(t2.t1_duration_months ~ pncGrpPsychosis + ageT1 + sex, tp.two.data)
mod1A <- aov(mod1)

## Looks like we have differences now I need to plot em
meanVals.t2 <- summarySE(data=tp.two.data, measurevar='t2.t1_duration_months', groupvars=c('pncGrpPsych'))
# Now plot these guys
outPlot <- ggplot(meanVals.t2, aes(x=pncGrpPsych, y=t2.t1_duration_months, )) +
  geom_bar(stat="identity", position=position_dodge(), size=.1) + 
                           geom_errorbar(aes(ymin=t2.t1_duration_months-se, ymax=t2.t1_duration_months+se), 
                           width = .2, position=position_dodge(.9)) + 
                           ylab("Mean duration")


## Now explore the maximum time frame I need to remove in order to make our differences non significant
initRM <- 2
cutVals <- sort(unique(tp.two.data$t2.t1_duration_months), decreasing=T)
sigVal <- matrix(unlist(summary(mod1)), ncol=5, nrow=4)[1,5]
while(sigVal < .05){
  tmpData <- tp.two.data[-which(tp.two.data$t2.t1_duration_months>cutVals[initRM]),]
  mod1 <- aov(t2.t1_duration_months ~ pncGrpPsychosis + ageT1 + sex, tmpData)
  sigVal <- matrix(unlist(summary(mod1)), ncol=5, nrow=4)[1,5]
  initRM <- initRM + 1
}
