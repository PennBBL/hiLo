## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table')

# Declare any functions
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

# Now load the data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
reho.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')

# Now add age bins
cbf.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(cbf.modal.data.age.reg))
vol.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(vol.modal.data.age.reg))
gmd.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(gmd.modal.data.age.reg))
reho.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(reho.modal.data.age.reg))
alff.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(alff.modal.data.age.reg))
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

# Now create a static perf bin variable
tmpDF <- vol.modal.data.age.reg
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

## Now lets produce all of our effect sizes
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg,'mprage_jlf_vol', 0, 999,'Age Regressed', cerebellum=T,optionalRace=NULL)[,c('ROI_readable','zScoreDifference','sex')]
age.reg.cbf <- doEverythingEver(cbf.modal.data.age.reg,'pcasl_jlf_cbf', 0, 999,'Age Regressed', cerebellum=F,optionalRace=NULL)[,c('ROI_readable','zScoreDifference','sex')]
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg,'mprage_jlf_gmd', 0, 999,'Age Regressed', cerebellum=T,optionalRace=NULL)[,c('ROI_readable','zScoreDifference','sex')]
age.reg.rh <- doEverythingEver(reho.modal.data.age.reg,'rest_jlf_reho', 0, 999,'Age Regressed', cerebellum=T,optionalRace=NULL)[,c('ROI_readable','zScoreDifference','sex')]
age.reg.al <- doEverythingEver(alff.modal.data.age.reg,'rest_jlf_alff', 0, 999,'Age Regressed', cerebellum=T,optionalRace=NULL)[,c('ROI_readable','zScoreDifference','sex')]
age.reg.tr <- doEverythingEver(tr.modal.data.age.reg, 'dti_jlf_tr', 0, 999,'Age Regressed', cerebellum=T,optionalRace=NULL)[,c('ROI_readable','zScoreDifference','sex')]

## Now make sure our output conforms to everything else
output <- rbind(age.reg.vol, age.reg.cbf, age.reg.gmd, age.reg.rh, age.reg.al, age.reg.tr)

## Now onto WM labels
# Change the workhorse function
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
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabel.csv')
colnames(fa.data)[85] <- 'dti_dtitk_jhulabel_fa_rlic'
fa.data$ageBin <- 'Age Regressed'
fa.data.age.reg <- doEverythingEver(fa.data, 'dti_dtitk_jhulabel_fa', 0, 167, 'Age Regressed')[,c('ROI','zScoreDifference','sex')]
fa.data.age.reg$ROI <- paste('dti_dtitk_jlf_fa_', fa.data.age.reg$ROI, sep='')
colnames(fa.data.age.reg)[1] <- 'ROI_readable'

output <- rbind(output, fa.data.age.reg)
write.csv(output, "effSizeImp.csv", quote=F, row.names=F)
