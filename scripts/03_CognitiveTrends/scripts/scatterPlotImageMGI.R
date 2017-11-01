## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/volumeData.csv')
vol.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- vol.modal.data.age.reg$Overall_EfficiencyAR
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/gmdData.csv')
gmd.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- gmd.modal.data.age.reg$Overall_EfficiencyAR
ct.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/ctData.csv')
ct.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- ct.modal.data.age.reg$Overall_EfficiencyAR
cc.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/ccData.csv')
cc.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- cc.modal.data.age.reg$Overall_EfficiencyAR

vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
ct.modal.data.age.reg$ageBin <- 'Age Regressed'
cc.modal.data.age.reg$ageBin <- 'Age Regressed'

data.names <- c('vol','gmd', 'ct', 'cc')
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_','mprage_jlf_ct_', 'mprage_jlf_cortcon_')
wmAdd <- c(1,0)
cerebellumValues <- c('TRUE', 'TRUE')
pdf('ageRegressedMGI.pdf')
for(q in 1:2){
  print(data.names[q])
  dfName <- paste(data.names[q], ".modal.data.age.reg", sep='')
  data <- doEverythingEver(get(dfName), grepVals[q], 0, 999, 'Age Regressed', cerebellumIn=cerebellumValues[q], optionalRace=NULL)
  if(wmAdd[q]==1){
  data <- rbind(data, doEverythingEverWM(get(dfName), grepVals[q], 0, 999, 'Age Regressed', cerebellumIn=cerebellumValues[q], optionalRace=NULL)) 
  }
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
# Now do CT and CC
for(q in 3:4){
  dfName <- paste(data.names[q], ".modal.data.age.reg", sep='')
  data <- doEverythingEverCT(get(dfName), grepVals[q], 0, 999, 'Age Regressed', optionalRace=NULL)
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
dev.off()
