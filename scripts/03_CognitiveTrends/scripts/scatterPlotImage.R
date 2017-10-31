## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

# Declare any functions
# Now load the data
vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
cc.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ccData.csv')
reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')
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

# Now add age bins
vol.modal.data <- addAgeBin(vol.modal.data, vol.modal.data$ageAtGo1Scan, 167, 215, 216)
cbf.modal.data <- addAgeBin(cbf.modal.data, cbf.modal.data$ageAtGo1Scan, 167, 215, 216)
gmd.modal.data <- addAgeBin(gmd.modal.data, gmd.modal.data$ageAtGo1Scan, 167, 215, 216)
ct.modal.data <- addAgeBin(ct.modal.data, ct.modal.data$ageAtGo1Scan, 167, 215, 216)
cc.modal.data <- addAgeBin(cc.modal.data, cc.modal.data$ageAtGo1Scan, 167, 215, 216)
reho.modal.data <- addAgeBin(reho.modal.data, reho.modal.data$ageAtGo1Scan, 167, 215, 216)
alff.modal.data <- addAgeBin(alff.modal.data, alff.modal.data$ageAtGo1Scan, 167, 215, 216)
ad.modal.data <- addAgeBin(ad.modal.data, ad.modal.data$ageAtGo1Scan, 167, 215, 216)
fa.modal.data <- addAgeBin(fa.modal.data, fa.modal.data$ageAtGo1Scan, 167, 215, 216)
rd.modal.data <- addAgeBin(rd.modal.data, rd.modal.data$ageAtGo1Scan, 167, 215, 216)
tr.modal.data <- addAgeBin(tr.modal.data, tr.modal.data$ageAtGo1Scan, 167, 215, 216)
cbf.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(cbf.modal.data.age.reg))
vol.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(vol.modal.data.age.reg))
gmd.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(gmd.modal.data.age.reg))
#gmd.modal.data.age.reg <- addAgeBin(gmd.modal.data.age.reg, gmd.modal.data.age.reg$ageAtGo1Scan, 167, 215, 216)
ct.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(ct.modal.data.age.reg))
cc.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(cc.modal.data.age.reg))
reho.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(reho.modal.data.age.reg))
alff.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(alff.modal.data.age.reg))
ad.modal.data.age.reg$ageBin <- 'Age Regressed'
fa.modal.data.age.reg$ageBin <- 'Age Regressed'
rd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

# Start with the age regressed data 
data.names <- c('vol', 'cbf', 'gmd', 'reho', 'alff', 'tr', 'ct', 'cc')
grepVals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_tr_', 'mprage_jlf_ct_', 'mprage_jlf_cortcon_')
wmAdd <- c(1,1,0,0,0,1)
cerebellumValues <- c('TRUE', 'FALSE', 'TRUE', 'TRUE', 'TRUE', 'TRUE')
pdf('ageRegressed.pdf')
for(q in 1:6){
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
for(q in 7:length(data.names)){
  dfName <- paste(data.names[q], ".modal.data.age.reg", sep='')
  data <- doEverythingEverCT(get(dfName), grepVals[q], 0, 999, 'Age Regressed', optionalRace=NULL)
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
dev.off()

# Now go through the age bins 
 # Start with childhood
#child.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 0, 167, 'Childhood', cerebellum=F,optionalRace=NULL)
pdf('Childhood.pdf')
for(q in 1:6){
  print(data.names[q])
  dfName <- paste(data.names[q], ".modal.data", sep='')
  data <- doEverythingEver(get(dfName), grepVals[q], 0, 167, 'Childhood', cerebellum=cerebellumValues[q], optionalRace=NULL)
  if(wmAdd[q]==1){
  data <- rbind(data, doEverythingEverWM(get(dfName), grepVals[q], 0, 167, 'Childhood', cerebellumIn=cerebellumValues[q], optionalRace=NULL)) 
  }
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
# Now do CT and CC
for(q in 7:length(data.names)){
  dfName <- paste(data.names[q], ".modal.data", sep='')
  data <- doEverythingEverCT(get(dfName), grepVals[q], 0, 167, 'Childhood', optionalRace=NULL)
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
dev.off()

# Now adolescent
#doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 168, 215, 'Adolescence', cerebellum=F,optionalRace=NULL)
pdf('Adolescence.pdf')
for(q in 1:6){
  print(data.names[q])
  dfName <- paste(data.names[q], ".modal.data", sep='')
  data <- doEverythingEver(get(dfName), grepVals[q], 168, 215, 'Adolescence', cerebellum=cerebellumValues[q], optionalRace=NULL)
  if(wmAdd[q]==1){
  data <- rbind(data, doEverythingEverWM(get(dfName), grepVals[q], 168, 215, 'Adolescence', cerebellumIn=cerebellumValues[q], optionalRace=NULL)) 
  }
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
# Now do CT and CC
for(q in 7:length(data.names)){
  dfName <- paste(data.names[q], ".modal.data", sep='')
  data <- doEverythingEverCT(get(dfName), grepVals[q], 168, 215, 'Adolescence', optionalRace=NULL)
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
dev.off()

# And finally early adulthood
#adult.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 216, 999, 'Early Adulthood', cerebellum=F,optionalRace=NULL)
pdf('EarlyAdulthood.pdf')
for(q in 1:6){
  print(data.names[q])
  dfName <- paste(data.names[q], ".modal.data", sep='')
  data <- doEverythingEver(get(dfName), grepVals[q], 216, 999, 'Early Adulthood', cerebellum=cerebellumValues[q], optionalRace=NULL)
  if(wmAdd[q]==1){
  data <- rbind(data, doEverythingEverWM(get(dfName), grepVals[q], 216, 999, 'Early Adulthood', cerebellumIn=cerebellumValues[q], optionalRace=NULL)) 
  }
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
# Now do CT and CC
for(q in 7:length(data.names)){
  dfName <- paste(data.names[q], ".modal.data", sep='')
  data <- doEverythingEverCT(get(dfName), grepVals[q], 216, 999, 'Early Adulthood', optionalRace=NULL)
  outPlot <- scatMan(data, data.names[q])
  print(outPlot)
}
dev.off()
