## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table')

## Load data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/jlfTRData.csv')

## Prep data
cbf.modal.data.age.reg$ageBin <- 'Age Regressed'
vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

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

## Create our static perf bin
tmpDF <- vol.modal.data.age.reg
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

# Now I need to grab the effect sizes
## Now prep the data 
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.vol$modal <- 'Volume'
age.reg.cbf <- doEverythingEver(cbf.modal.data.age.reg, 'pcasl_jlf_cbf', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.cbf$modal <- 'CBF'
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.gmd$modal <- 'GMD'
age.reg.tr <- doEverythingEver(tr.modal.data.age.reg, 'dti_jlf_tr', 0, 167, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.tr$modal <- 'TR'

# Now I need to prepare the values for one graph
toPlot <- rbind(age.reg.vol, age.reg.cbf, age.reg.gmd, age.reg.tr)
toPlot$modal <- factor(toPlot$modal, levels=c('Volume', 'GMD', 'CBF', 'TR'))
plotToReturn <- ggplot(toPlot, aes(y=zScoreDifference, x=ROI, group=sex, color=sex)) +
      geom_line( size=5) +
      geom_point(size=1.5) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "sex",
                          values=c("M"="blue1","F"="red1")) +
      theme_bw() +
      theme(legend.position="top") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
      coord_cartesian(ylim = c(-1, 1)) + 
      facet_grid(modal ~ lobe, scales="free", space="free_x") +
      scale_y_continuous(limits=c(-1, 1), 
                           breaks=round(seq(-1,1,.25), digits=2))

pdf('testingHiLoEffectSize.pdf', height=20, width=20)
plotToReturn
dev.off()

# Now do Reho
# Load the data 
reho.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/rehoData.csv')

# Add the age bin 
reho.modal.data.age.reg$ageBin <- 'Age Regressed'

# Grab the z scores
age.reg.reho <- doEverythingEver(reho.modal.data.age.reg, 'rest_jlf_reho', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.reho$modal <- 'ReHo'
toPlot <- age.reg.reho
plotToReturn <- ggplot(toPlot, aes(y=zScoreDifference, x=ROI, group=sex, color=sex)) +
      geom_line( size=5) +
      geom_point(size=1.5) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "group",
                          values=c("M"="blue1","F"="red1")) +
      theme_bw() +
      theme(legend.position="top") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
      coord_cartesian(ylim = c(-1, 1)) + 
      facet_grid(modal ~ lobe, scales="free", space="free_x") +
      scale_y_continuous(limits=c(-1, 1), 
                           breaks=round(seq(-1,1,.25), digits=2))

pdf('testingHiLoEffectSizeReho.pdf', height=6, width=20)
plotToReturn
dev.off()

# Now create the wm tract values
# Create the do everything function for WM tracts
doEverythingEver <- function(df, modalityGrepPattern, lowerAge.e, upperAge.e, ageBinName.e){
  #tmp <- addAgeBins(df$ageAtGo1Scan, df, lowerAge.e, upperAge.e, ageBinName.e)
  tmp <- standardizePerfGroups(df, modalityGrepPattern, ageBinName.e)
  tmp <- organizeWM1ROINames(tmp)
  tmp <- subtractHiFromLo(tmp)
  tmp$gender <- revalue(tmp$gender, c('1'='Male', '2'='Female'))
  tmp$meanValue <- as.numeric(as.character(tmp$meanValue))
  colnames(tmp)[6] <- 'Gender'
  levels(tmp$ageBin) <- rev(levels(tmp$ageBin))
  return(tmp)
}

fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/jhuFALabel.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegQA/jhuTRLabel.csv')
fa.data$ageBin <- 'Age Regressed'
tr.data$ageBin <- 'Age Regressed'
fa.data.age.reg <- doEverythingEver(fa.data, 'dti_dtitk_jhulabel_fa', 0, 999, 'Age Regressed')
fa.data.age.reg$modal <- 'FA'
tr.data.age.reg <- doEverythingEver(tr.data, 'dti_dtitk_jhulabel_tr', 0, 999, 'Age Regressed')
tr.data.age.reg$modal <- 'TR'

# Now plot em
# Now I need to prepare the values for one graph
toPlot <- rbind(fa.data.age.reg, tr.data.age.reg)
toPlot$modal <- factor(toPlot$modal, levels=c('FA', 'TR'))
plotToReturn <- ggplot(toPlot, aes(y=zScoreDifference, x=ROI, group=sex, color=sex)) +
      geom_line( size=5) +
      geom_point(size=1.5) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "sex",
                          values=c("M"="blue1","F"="red1")) +
      theme_bw() +
      theme(legend.position="top") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
      coord_cartesian(ylim = c(-1, 1)) + 
      facet_grid(modal ~ lobe, scales="free", space="free_x") +
      scale_y_continuous(limits=c(-1, 1), 
                           breaks=round(seq(-1,1,.25), digits=2))


pdf('testingHiLoEffectSizeWMLabels.pdf', height=10, width=12)
plotToReturn
dev.off()
