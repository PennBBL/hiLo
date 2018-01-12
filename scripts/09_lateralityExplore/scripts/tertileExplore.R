## Load library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/09_lateralityExplore/functions/functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table')

## Load data
toUse <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/gmdData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfTRData.csv')

## Prep data
cbf.data$ageBin <- 'Age Regressed'
vol.data$ageBin <- 'Age Regressed'
gmd.data$ageBin <- 'Age Regressed'
tr.data$ageBin <- 'Age Regressed'

## Declare any functions
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.33,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 'lo'
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 'me'
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 'hi'
  return(data)
}

#Modify the do everything function so we don't subtract high minus low values and maintain a laterality component
doEverythingEver <- function(df, modalityGrepPattern, ageBinName.e, cerebellumIn=F){
  hemiVals <- c(paste(modalityGrepPattern, 'R', sep=''), paste(modalityGrepPattern, 'L', sep=''))
  allVals <- NULL
  for(i in 1:2){
    tmp <- standardizePerfGroups(df, hemiVals[i], ageBinName.e)
    tmp <- organizeROINames(tmp, cerebellum=cerebellumIn)
    dimVal <- dim(tmp)
    allVals <- rbind(allVals, tmp)
  }
  tmp <- allVals  
  tmp$gender <- revalue(tmp$gender, c('1'='Male', '2'='Female'))
  tmp$meanValue <- as.numeric(as.character(tmp$meanValue))
  colnames(tmp)[6] <- 'Gender'
  tmp$ageBin <- factor(tmp$ageBin, levels=c('Early Adulthood','Adolescence','Childhood', 'Age Regressed'))
  # Now split and subtract the differences
  rTmp <- tmp[1:dimVal[1],]
  rTmp <- rTmp[which(rTmp$groupLevel!='me'),]
  lTmp <- tmp[-seq(1, dimVal[1]),]
  lTmp <- lTmp[which(lTmp$groupLevel!='me'),]
  # Now find the differences
  tmp <- rTmp
  tmp$meanValue <- lTmp$meanValue - rTmp$meanValue
  tmp$Gender2 <- as.factor(paste(tmp$Gender, tmp$groupLevel))
  return(tmp)
}

## Create our static perf bin
tmpDF <- vol.data
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

age.reg.vol <- doEverythingEver(vol.data, 'mprage_jlf_vol_','Age Regressed', cerebellum=F)

outputPlot <- ggplot(age.reg.vol[which(age.reg.vol$Gender=='Male'),], aes(x=ROI, y=meanValue, group=groupLevel, color=Gender2)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  scale_y_continuous(limits=c(-.3, .3), breaks=round(seq(-.3,.3,.2), digits=2)) + 
  xlab("ROI") + 
  ylab("L - R") + 
  geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) + 
  theme_bw() +
  theme(legend.position="top", text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
  facet_grid(. ~ lobe, scales="free", space="free_x")
