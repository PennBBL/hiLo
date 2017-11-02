## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

# Load data 
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cc.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')

# Now add age bin
cbf.modal.data.age.reg$ageBin <- 'Age Regressed'
vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
ct.modal.data.age.reg$ageBin <- 'Age Regressed'
cc.modal.data.age.reg$ageBin <- 'Age Regressed'
reho.modal.data.age.reg$ageBin <- 'Age Regressed'
alff.modal.data.age.reg$ageBin <- 'Age Regressed'
ad.modal.data.age.reg$ageBin <- 'Age Regressed'
fa.modal.data.age.reg$ageBin <- 'Age Regressed'
rd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

# Now create a scatter plot function
scatterPlotFunction <- function(allDataVals, xLab, yLab){
  rangeX <- round(range(allDataVals[,2]), digits=2)
  rangeY <- round(range(allDataVals[,3]), digits=2)
  corVal <- paste("r = ", round(cor(allDataVals[,2], allDataVals[,3]), digits=2))
  plotOut <-  ggplot(allDataVals, aes(x=zScoreDifference.x, y=zScoreDifference.y)) + 
    geom_point(aes(fill=lobe)) + 
    geom_smooth(method=lm) + 
    geom_label_repel(aes(label=ROI,color=lobe,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
    coord_cartesian(xlim=c(rangeX[1]-.1, rangeX[2]+.1), ylim=c(rangeY[1]-.1,rangeY[2]+.1)) + 
    xlab(xLab) + 
    ylab(yLab) + 
    geom_hline(yintercept = 0 , linetype=3) + 
    geom_vline(xintercept = 0 , linetype=3) + 
    theme(legend.position="none") + 
  geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))
  if(cor(allDataVals$zScoreDifference.x, allData$zScoreDifference.y) > 0){
    plotOut <- plotOut + geom_abline(intercept=0, slope=1)
  }
  if(cor(allDataVals$zScoreDifference.x, allData$zScoreDifference.y) < 0){
    plotOut <- plotOut + geom_abline(intercept=0, slope=-1)
  }
  return(plotOut)
}

# Now w/in a loop I need to 
data.names <- c('vol', 'cbf', 'gmd','reho', 'alff', 'tr')
grepPattern1 <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','rest_jlf_reho_', 'rest_jlf_alff_','dti_jlf_tr_')
for(q in 1:length(grepPattern1)){
  modalName <- rev(strSplitMatrixReturn(grepPattern1[q], '_'))[1]
  pdf(paste(modalName, '-interModalCor.pdf', sep=''))
  for(n in 1:length(grepPattern1)){
    dfOneName <- get(paste(data.names[q], ".modal.data.age.reg", sep=''))
    dfTwoName <- get(paste(data.names[n], ".modal.data.age.reg", sep=''))
    valuesOne <-  doEverythingEver(dfOneName, grepPattern1[q], 0, 999, 'Age Regressed', cerebellum=F)
    valuesTwo <- doEverythingEver(dfTwoName, grepPattern1[n], 0, 999, 'Age Regressed', cerebellum=F)
    meanValsOne <- summarySE(valuesOne, measurevar='zScoreDifference', groupvars='ROI')[,c(1,3)]
    meanValsTwo <- summarySE(valuesTwo, measurevar='zScoreDifference', groupvars='ROI')[,c(1,3)]
    allData <- merge(meanValsOne, meanValsTwo, by='ROI')
    allData <- merge(allData, valuesOne, by='ROI')
    allData <- allData[duplicated(allData$ROI),]
    outPlot <- scatterPlotFunction(allData, data.names[q], data.names[n])
    print(outPlot)
  }
  dev.off()
}

# Now do CT
for(q in 1){
  modalName <- 'ct'
  pdf(paste(modalName, '-interModalCor.pdf', sep=''))
  for(n in 1:length(grepPattern1)){
    dfOneName <- get(paste("ct.modal.data.age.reg", sep=''))
    dfTwoName <- get(paste(data.names[n], ".modal.data.age.reg", sep=''))
    valuesOne <-  doEverythingEverCT(dfOneName, 'mprage_jlf_ct_', 0, 999, 'Age Regressed')
    valuesTwo <- doEverythingEver(dfTwoName, grepPattern1[n], 0, 999, 'Age Regressed', cerebellum=F)
    meanValsOne <- summarySE(valuesOne, measurevar='zScoreDifference', groupvars='ROI')[,c(1,3)]
    meanValsTwo <- summarySE(valuesTwo, measurevar='zScoreDifference', groupvars='ROI')[,c(1,3)]
    allData <- merge(meanValsOne, meanValsTwo, by='ROI')
    allData <- merge(allData, valuesOne, by='ROI')
    allData <- allData[duplicated(allData$ROI),]
    outPlot <- scatterPlotFunction(allData, 'ct', data.names[n])
    print(outPlot)
  }
  dev.off()
}
