## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/10_hiLoExtension/functions/functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

## Load data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
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

doEverythingEver <- function(df, modalityGrepPattern, lowerAge.e, upperAge.e, ageBinName.e, cerebellumIn=F, optionalRace=NULL, optionalFactor="F1_Exec_Comp_Cog_Accuracy"){
  if(!identical(optionalRace, NULL)){
    df <- df[which(df$race2==optionalRace),]
  }
  tmp <- standardizePerfGroups(df, modalityGrepPattern, ageBinName.e, optionalFactor)
  tmp <- organizeROINames(tmp, cerebellum=cerebellumIn)
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

# Produce the effect sizes for every factor value
fact.vals <- names(vol.modal.data.age.reg)[4:16]
orig <- age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL, optionalFactor=fact.vals[1]) 
for(f in fact.vals){
  age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL, optionalFactor=f)
  orig <- merge(orig, age.reg.vol, by=c('ROI', 'Gender', 'sex', 'groupLevel', 'ageBin', 'ROI_readable', 'lobe'), suffixes=c("", f))
}
origM <- orig[which(orig$sex=="M"),]
origF <- orig[which(orig$sex=="F"),]

# Now compare the new hi - lo values to the f1 exec comp cog acc values
valsIn <- grep("zScoreDifference", names(orig))[-1]
pdf("scatterPlotMale.pdf", height=20, width=20)
for(f in valsIn){
  # Make a ggplot scatter image as per usual
  corVal <- paste("r = ", round(cor(origM$zScoreDifferenceF1_Exec_Comp_Cog_Accuracy, origM[,f]), digits=2))
  colVal <- colnames(orig)[f]
  outPlot <-  ggplot(origM, aes(x=zScoreDifferenceF1_Exec_Comp_Cog_Accuracy, y=origM[,f])) + 
    geom_point(aes(fill=lobe)) +
    geom_smooth(method=lm) +
    ylab(colVal) +
    geom_label_repel(aes(label=ROI,color=lobe,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
    geom_abline(intercept=0, slope=1) + 
    geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))
    
  print(outPlot)
}
dev.off()

# Now do the females
pdf("scatterPlotFemale.pdf")
for(f in valsIn){
  # Make a ggplot scatter image as per usual
  corVal <- paste("r = ", round(cor(origF$zScoreDifferenceF1_Exec_Comp_Cog_Accuracy, origF[,f]), digits=2))
  colVal <- colnames(orig)[f]
  outPlot <-  ggplot(origF, aes(x=zScoreDifferenceF1_Exec_Comp_Cog_Accuracy, y=origF[,f])) + 
    geom_point(aes(fill=lobe)) +
    geom_smooth(method=lm) +
    ylab(colVal) +
    geom_label_repel(aes(label=ROI,color=lobe,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines")) + 
    geom_abline(intercept=0, slope=1) + 
    geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))
    
  print(outPlot)
}
dev.off()

# Now produce a heat map for all of this
inVal <- melt(cor(origM[,grep("zScoreDifference", names(orig))[-1]]))
inVal$Var1 <- gsub(inVal$Var1, pattern='zScoreDifference', replacement='')
inVal$Var2 <- gsub(inVal$Var2, pattern='zScoreDifference', replacement='')
outMat1 <- qplot(x=Var1, y=Var2, data=inVal, fill=value, geom="tile") +
  theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    scale_fill_gradient2(low="blue", high="red", limits=c(-1, 1))
inVal <- melt(cor(origF[,grep("zScoreDifference", names(orig))[-1]]))
inVal$Var1 <- gsub(inVal$Var1, pattern='zScoreDifference', replacement='')
inVal$Var2 <- gsub(inVal$Var2, pattern='zScoreDifference', replacement='')
outMat2 <- qplot(x=Var1, y=Var2, data=inVal, fill=value, geom="tile") +
  theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    scale_fill_gradient2(low="blue", high="red", limits=c(-1, 1))

# Now wirte the cor mats
pdf("corMatrices.pdf")
print(outMat1)
print(outMat2)
dev.off()

# Now writew the orig values
write.csv(orig, "outputHiLoEffectsVolume.csv", quote=F, row.names=F)
