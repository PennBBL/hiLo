## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

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

# Now create a static perf bin variable
tmpDF <- vol.modal.data.age.reg
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

# Start with the age regressed data 
data.names <- c('vol', 'cbf', 'gmd', 'reho', 'alff', 'tr')
#data.names <- c('reho','alff','tr')
grepVals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_tr_', 'mprage_jlf_ct_', 'mprage_jlf_cortcon_')
#grepVals <- c('rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_tr_')
wmAdd <- c(1,1,0,0,0,1)
#wmAdd <- c(0,0,1,0,0,1)
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
  ## Now merge these values based on ROI name
  if(q==1){
    mergeTarg <- data
  }
  if(q>1){
    mergeTarg <- merge(mergeTarg, data, by=c('ROI','groupLevel','Gender'),suffixecs=c('',grepVals[q]), all=T)
  }
}
dev.off()

## Now create an abs sum of all of these metrics
mergeTarg$absSum <- rowSums(abs(mergeTarg[,grep("zScoreDifference", names(mergeTarg))]), na.rm=T)
mergeTarg <- reshape(data=mergeTarg, direction="wide", idvar="ROI", timevar='Gender', v.names='absSum')

## Now plot these values
pdf('absSumEF.pdf', width=12, height=12)
corVal <- paste("r = ", round(cor(as.numeric(as.character(mergeTarg$absSum.Female)), as.numeric(as.character(mergeTarg$absSum.Male))), digits=2))
plotOut <-  ggplot(mergeTarg, aes(y=absSum.Female, x=absSum.Male)) + 
geom_point(aes(fill=lobe.x)) + 
  geom_smooth(method=lm) + 
  geom_label_repel(aes(label=ROI,color=lobe.x,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
  xlab("Male z-score difference") + 
  ylab("Female z-score difference") + 
  geom_hline(yintercept = 0 , linetype=3) + 
  geom_vline(xintercept = 0 , linetype=3) + 
  theme(legend.position="none") + 
  geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal)) + geom_abline(intercept=0, slope=1)
#plotOut


cohenD <- mergeTarg
source("../../08_importanceExplore/scripts/prepRidgeRegImport.R")

## Now plot cohenD vs ridge coef
toPlot <- merge(cohenD, mergeTarg, by='ROI',suffixes = c(".Cohen",".Ridge"))
write.csv(toPlot, "test.csv", quote=F, row.names=F)

## Now plot the male D vs Ridge coef
corVal1 <- paste("r = ", round(cor(as.numeric(as.character(toPlot$absSum.Male)), as.numeric(as.character(toPlot$sumAbs.Male))), digits=2))
## Now remove reho and alff

plotOutM <-  ggplot(toPlot, aes(y=absSum.Male, x=sumAbs.Male)) + 
  geom_point(aes(fill=lobe.x)) + 
  geom_smooth(method=lm) + 
  geom_label_repel(aes(label=ROI,color=lobe.x,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
  xlab("Ridge Coef") + 
  ylab("Cohen D") + 
  geom_hline(yintercept = 0 , linetype=3) + 
  geom_vline(xintercept = 0 , linetype=3) + 
  theme(legend.position="none") + 
  geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal1)) + geom_abline(intercept=0, slope=1) +
  ggtitle("Male Cohen D vs Ridge Coef")
corVal2 <- paste("r = ", round(cor(as.numeric(as.character(toPlot$absSum.Female)), as.numeric(as.character(toPlot$sumAbs.Female))), digits=2))
plotOutF <-  ggplot(toPlot, aes(y=absSum.Female, x=sumAbs.Female)) + 
  geom_point(aes(fill=lobe.x)) + 
  geom_smooth(method=lm) + 
  geom_label_repel(aes(label=ROI,color=lobe.x,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
  xlab("Ridge Coef") + 
  ylab("Cohen D") + 
  geom_hline(yintercept = 0 , linetype=3) + 
  geom_vline(xintercept = 0 , linetype=3) + 
  theme(legend.position="none") + 
  geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal2)) + geom_abline(intercept=0, slope=1) +
  ggtitle("Female Cohen D vs Ridge Coef")

plotOutM
plotOutF
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
