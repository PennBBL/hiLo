# AFGR May 2018 

# This score will be used to calculate propensity scores given the following formula:
# TD ~ ageATT1 + Duration to Terminal
# This will then give a propabbility for a age and duration profile of being that of a TD
# I will then use the inverse of this propensity score to weight a regression
# Thus favoring those that have a more similar to TD profile

## Load library(s)
install_load('ggplot2', 'lme4', 'lmerTest')

## Load the data
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-22.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")


## Now I need to isolate to only the long labels
all.data.tu <- all.data[which(all.data$bblid %in% names(which(table(all.data$bblid)>=2))),]

## Now create a variable to calculate the time from T1
# Now loop through each BBLID and find the time since time 1
## The first thing I need to do is find the time from T1 in days
all.data.tu$timeDiff <- 0
all.data.tu$ageScan1 <- 0
# Now loop through each BBLID and find the time since time 1
output.vol <- NULL
for(i in unique(all.data.tu$bblid)){
  ## Now add days from Scan 1
  tmpDat <- all.data.tu[which(all.data.tu$bblid==i),]
  print(dim(tmpDat))
  print(i)
  if(dim(tmpDat)[1]>1){
    minValue <- min(tmpDat$DOSCAN)
    maxValue <- max(tmpDat$tpvalue)
    tmpDat$timeDiff <- difftime(tmpDat$DOSCAN, minValue, units=c("day"))
    tmpDat$ageScan1 <- tmpDat$scanageMonths[which(tmpDat$tpvalue==1)]
    tmpDat <- tmpDat[which(tmpDat$tpvalue==maxValue),]
  }
  # Now combine everything
  output.vol <- rbind(output.vol, tmpDat)
}
## Now create a binary TD !TD outcome
all.data.tu.min <- output.vol
all.data.tu.min$propScoreOutcome <- 0
all.data.tu.min$propScoreOutcome[which(all.data.tu.min$pncGrpPsychosis!="TD")] <- 1
all.data.tu.min <- all.data.tu.min[which(complete.cases(all.data.tu.min$ageScan1) & complete.cases(all.data.tu.min$timeDiff)),]
all.data.tu.min <- all.data.tu.min[which(all.data.tu.min$pncGrpPsychosis!=''),]

## Now train my logistic regression model
prop.model <- glm(propScoreOutcome ~ ageScan1 + timeDiff, data=all.data.tu.min, family='binomial')

## Now grab our scores!
all.data.tu.min$propScore <- predict(prop.model, type='response')

## Now create a histogram for these probs
tmpPlot <- ggplot(all.data.tu.min, aes(x=propScore, fill=pncGrpPsychosis)) +
  geom_histogram(bins=50)
tmpPlot <- ggplot(all.data.tu.min, aes(x=1/propScore, fill=pncGrpPsychosis)) +
  geom_histogram(bins=50)

## Now a train a model using the inverse of these guys as the weights
all.data.tu$propScore <- all.data.tu.min$propScore[match(all.data.tu$bblid, all.data.tu.min$bblid)]
all.data.tu.mem <- all.data.tu[complete.cases(all.data.tu$propScore),]
all.data.tu.mem$propScoreWeights <- 1/all.data.tu.mem$propScore
all.data.tu.mem <- all.data.tu.mem[which(all.data.tu.mem$pncGrpPsychosis=="Persister" | all.data.tu.mem$pncGrpPsychosis=="Resilient"),]

## Now build a model for these guys
mem.test <- lmer(mprage_jlf_vol_TBWM~scanageMonths*pncGrpPsychosis+sex+(1|bblid), REML=T, weights=propScoreWeights,data=all.data.tu.mem)

## Now make a spaghetti plot for all of our summary metrics
summaryMetrics <- names(all.data)[1523:1534][-c(3,4,5,7,8)]
pdf('sigInteractions.pdf')
tmpPlot <- ggplot(all.data.tu.mem, aes(x=propScore, fill=pncGrpPsychosis)) +
  geom_histogram(bins=50)
print(tmpPlot)
for(i in summaryMetrics){
  tmpData <- all.data.tu.mem[complete.cases(all.data.tu.mem[,c(i,'sex','bblid')]),]
  tmpFor <- as.formula(paste(i, "~sex+(1|bblid)", sep=''))
  tmpMod <- lmer(tmpFor, REML=T, weights=propScoreWeights,data=tmpData)
  tmpData$tmpVals <- scale(residuals(tmpMod))
  tmpPlot <- ggplot(tmpData, aes(x=scanageMonths, y=tmpVals, col=pncGrpPsychosis)) +
    geom_point() + 
    geom_line(aes(group=bblid)) +
    geom_smooth(method='lm') +
    ylab(i)
  tmpFor <- as.formula(paste(i, "~scanageMonths*pncGrpPsychosis+sex+(1|bblid)", sep=''))
  testMod <- lmer(tmpFor, REML=T, weights=propScoreWeights,data=all.data.tu.mem)
  tmpVals <- summary(testMod)
  if(foo$coefficients['scanageMonths:pncGrpPsychosisResilient', 'Pr(>|t|)']<.07){
    print(tmpPlot)
  }
}
dev.off()
