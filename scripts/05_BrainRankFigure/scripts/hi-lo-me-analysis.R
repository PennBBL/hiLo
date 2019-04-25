## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
source("/home/adrose/hiLo/scripts/05_BrainRankFigure/functions/functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'psych')

## Load data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegQA/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/gmdData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegQA/jlfTRData.csv')
alff.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegQA/alffData.csv')
reho.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegQA/rehoData.csv')

## Prep data
cbf.modal.data.age.reg$ageBin <- 'Age Regressed'
vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'
alff.modal.data.age.reg$ageBin <- 'Age Regressed'
reho.modal.data.age.reg$ageBin <- 'Age Regressed'

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

## Now modify our do everything ever function
doEverythingEver <- function(df, modalityGrepPattern, lowerAge.e, upperAge.e, ageBinName.e, cerebellumIn=F){
  tmp <- standardizePerfGroups(df, modalityGrepPattern, ageBinName.e)
  #tmp <- organizeROINames(tmp, cerebellum=cerebellumIn)
  output <- subtractHiFromLo(tmp)
  #tmp$gender <- revalue(tmp$gender, c('1'='Male', '2'='Female'))
  #tmp$meanValue <- as.numeric(as.character(tmp$meanValue))
  #colnames(tmp)[6] <- 'Gender'
  #tmp$roiFinal <- strSplitMatrixReturn(tmp$ROI_readable, modalityGrepPattern)
  #output <- tmp[,c('roiFinal', 'zScoreDifference', 'Gender')]
  return(output)
}

## And additionally add a WM lobe effect size do everything ever
doEverythingEverWM <- function(df, modalityGrepPattern, lowerAge.e, upperAge.e, ageBinName.e, cerebellumIn=F, optionalRace=NULL){
  if(!identical(optionalRace, NULL)){
    df <- df[which(df$race2==optionalRace),]
  }
  tmp <- standardizePerfGroups(df, modalityGrepPattern, ageBinName.e)
  tmp <- organizeWM2ROINames(tmp)
  tmp <- subtractHiFromLo(tmp)
  tmp$gender <- revalue(tmp$gender, c('1'='Male', '2'='Female'))
  tmp$meanValue <- as.numeric(as.character(tmp$meanValue))
  colnames(tmp)[6] <- 'Gender'
  tmp$ageBin <- factor(tmp$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))
  return(tmp)
}

## Create our static perf bin
tmpDF <- vol.modal.data.age.reg
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

## Now load the performance groups I have sent Ruben in the past
tmpDF <- read.csv("/home/gur/gursas/GO/perfBinsForRuben.csv")
tmpDF <- tmpDF[,1:3]
colnames(tmpDF)[3] <- 'groupFactorLevel'
tmpDF$groupFactorLevel <- plyr::revalue(factor(tmpDF$groupFactorLevel), c("1"="lo", "2"="me","3"="hi"))
static.perf.bin <- tmpDF

# Now I need to grab the effect sizes
## Now prep the data 
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol_', 0, 999, 'Age Regressed', cerebellum=T)
age.reg.cbf <- doEverythingEver(cbf.modal.data.age.reg, 'pcasl_jlf_cbf_', 0, 999, 'Age Regressed', cerebellum=F)
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd_', 0, 999, 'Age Regressed', cerebellum=T)
age.reg.tr <- doEverythingEver(tr.modal.data.age.reg, 'dti_jlf_tr_', 0, 167, 'Age Regressed', cerebellum=T)
age.reg.alff <- doEverythingEver(alff.modal.data.age.reg, 'rest_jlf_alff_', 0, 167, 'Age Regressed', cerebellum=T)
age.reg.reho <- doEverythingEver(reho.modal.data.age.reg, 'rest_jlf_reho_', 0, 167, 'Age Regressed', cerebellum=T)

# Now write the output for each isolated gender
for(genZ in c('F', 'M')){
  for(modZ in c('vol', 'cbf', 'gmd', 'tr','alff','reho')){
    # Grab our data
    inputDat <- get(paste('age.reg.', modZ, sep=''))
    inputDat <- inputDat[which(inputDat$sex==genZ),]
    writeColorTableandKey(inputData=inputDat, inputColumn=8, outName=paste(genZ,modZ, sep=''), minTmp=c(-1,0), maxTmp=c(0,1))
  }  
}

## Now do the hi-me, me-lo and lo-me 
# First declare everything we are going to loop through
sexRep <- c(1, 2)
suffixRep <- c("mprage_jlf_vol_", "pcasl_jlf_cbf_", "mprage_jlf_gmd_", "dti_jlf_tr_", "rest_jlf_alff_","rest_jlf_reho_")
dataRep <- c('vol','cbf','gmd','tr',"alff","reho")
for(g in sexRep){
  for(p in 1:length(suffixRep)){
    dataName <- paste(dataRep[p], '.modal.data.age.reg', sep='')
    prefName <- suffixRep[p]
    tmpDat <- get(dataName)
    tmpDat <- tmpDat[which(tmpDat$sex==g),]
    outputVals <- calculateDeltaHiMeLo(tmpDat, prefName)
    # Now write every color table
    for(q in c(2,3,5)){
      outputName <- paste(g, p, q, sep='')
      writeColorTableandKey(outputVals, q, outputName, c(-.8, 0), c(0, .8))
    }
  }
}
