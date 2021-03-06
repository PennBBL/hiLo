# AFGR June 2017

# This script is going to be used to probe the best cognitive outcome for the hi lo project
# Essentially we will be looking for the highest pearson corellation amongst individual modalities and cog outcomes.
# This will be probed by abs mean corellation within genders with the age regressed data

# Load library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('corrplot')

# Load data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cortcon.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
vol.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv')
cbf.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/cbfData.csv')
gmd.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/gmdData.csv')
ct.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/ctData.csv')
cortcon.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/ccData.csv')
reho.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/rehoData.csv')
alff.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/alffData.csv')
ad.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfADData.csv')
fa.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfFAData.csv')
rd.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfRDData.csv')
tr.modal.data.age.reg.mr <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfTRData.csv')

# create a rough function to create our rows given a grep pattern and data frame
createCorRows <- function(dataFrame, grepPattern){
  factorNames <- c('Overall_Accuracy', 'Overall_Speed', 'Overall_Efficiency', 'F1_Exec_Comp_Cog_Accuracy',
  'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy','F1_Slow_Speed', 'F2_Memory_Speed', 
  'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency',
  'F3_Memory_Efficiency','F4_Executive_Efficiency')
  colsOfInterest <- grep(grepPattern, names(dataFrame))
  output <- matrix(NA, nrow=length(factorNames), ncol=length(colsOfInterest))

  for(i in 1:length(factorNames)){
    facName <- factorNames[i]
    colValF <- grep(facName, names(dataFrame))    
    for(d in 1:length(colsOfInterest)){
      colValM <- colsOfInterest[d]
      corVal <- cor(dataFrame[,colValF], dataFrame[,colValM], use='complete')
      output[i,d] <- corVal
    }    
  }
  output <- as.data.frame(output)
  colNameVals <- names(dataFrame)[colsOfInterest]
  colNameVals <- gsub(x=colNameVals, pattern=grepPattern, replacement='')
  colnames(output) <- colNameVals
  rownames(output) <- factorNames
  return(output)
}

# Now create a function to do the previous task by lobes
createCorRowsLobe <- function(dataFrame, grepPattern){
  factorNames <- c('Overall_Accuracy', 'Overall_Speed', 'Overall_Efficiency', 'F1_Exec_Comp_Cog_Accuracy',
  'F2_Social_Cog_Accuracy', 'F3_Memory_Accuracy','F1_Slow_Speed', 'F2_Memory_Speed', 
  'F3_Fast_Speed', 'F1_Social_Cognition_Efficiency', 'F2_Complex_Reasoning_Efficiency',
  'F3_Memory_Efficiency','F4_Executive_Efficiency')
  lobeNames <- c('Basal Ganglia', 'Limbic', 'Frontal Orbital', 
  'Frontal Dorsal', 'Temporal', 'Parietal', 'Occipital', 'Cerebellum', 'Other')
  colsOfInterest <- grep(grepPattern, names(dataFrame)) 
  nameVals <- names(dataFrame)[colsOfInterest]
  lobeVector <- NULL
  for(q in nameVals){
    tmpVal <- findLobe(q)
    lobeVector <- append(lobeVector, tmpVal)
  }
  # now produce our corVector  
  tmpVals <- createCorRows(dataFrame, grepPattern)
  # Now loop thorugh and grab the mean cor by lobe
  outputRows <- NULL
  for(o in 1:dim(tmpVals)[1]){
    outputCols <- NULL
    for(t in 1:max(unique(lobeVector))){
      tmpCorVal <- mean(as.numeric(tmpVals[o,which(lobeVector==t)]))
      outputCols <- append(outputCols, tmpCorVal)
    }
    outputRows <- rbind(outputRows, outputCols)
  }
  output <- as.data.frame(outputRows)
  colnames(output) <- sort(unique(lobeVector))
  rownames(output) <- factorNames
  return(output)
}

findLobe <- function(grepPattern){
  # Declare the rois that we will grep through
  rois<-c("Thal","Putamen","Caudate","Pallidum",  # Basal Ganglia
          "Accumbens", "BasFor", # Basal Ganglia
          "PHG","Hip","PIns","SCA","AIns", # Limbic
          "ACgG","PCgG","Ent","Amygdala","MCgG", # Limbic
          "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe", # Frontal Orbital
          "FRP", "LOrG", # Frontal Orbital
          "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
          "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
          "PCu","PoG","AnG","PO","SPL","MPrG", # Parietal
          "SMG","MPoG", # Parietal
          "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG", # Occiptal
          "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X") # Cerebellum


  # Declare the index key which corresponds to lobe values
  index.key <- c(1,7,17,28,33,42,50,58,62)

  # Now find where the pattern matches to the roi list
  for(pattern.match.variable in 1:length(rois)){
    nuclei.to.grep <- rois[pattern.match.variable]
    grep.output <- grep(nuclei.to.grep ,grepPattern)    
    if(!identical(integer(0), grep.output)){
      break
    }
    if(pattern.match.variable==61){
      pattern.match.variable <- 62
    }
  }
  lobe.group <- findInterval(pattern.match.variable, index.key)
  return(lobe.group)
}


# Now create a function which will prodiuce all of the summary metrics for each of the cor rows for each gender
roughAllFunction <- function(dataFrame2, grepPattern2, outName){
  genderVals <- c(1,2)
  for(q in genderVals){
    histName <- paste(outName,'gender-', q, '-hist.pdf', sep='')
    valsName <- paste(outName, 'gender-', q, '-vals.csv', sep='')
    tmpDF <- dataFrame2[which(dataFrame2$sex==q),]
    corMat <- createCorRows(tmpDF, grepPattern2)
    pdf(histName)
    for(z in 1:dim(corMat)[1]){
      hist(as.numeric(corMat[z,]), xlim=c(-.5, .5), main=rownames(corMat)[z], ylim=c(0,50))
    }  
    dev.off()     
    corMat <- cbind(corMat, t(apply(corMat, 1, summary)))
    write.csv(corMat, valsName, quote=F, row.names=T)   
  }
}

roughAllFunctionLobe <- function(dataFrame2, grepPattern2, outName){
  genderVals <- c(1,2)
  for(q in genderVals){
    histName <- paste(outName,'gender-', q, '-hist.pdf', sep='')
    valsName <- paste(outName, 'gender-', q, '-vals.csv', sep='')
    tmpDF <- dataFrame2[which(dataFrame2$sex==q),]
    corMat <- createCorRowsLobe(tmpDF, grepPattern2)
    pdf(histName)
    for(z in 1:dim(corMat)[1]){
      hist(as.numeric(corMat[z,]), xlim=c(-.5, .5), main=rownames(corMat)[z], ylim=c(0,50))
    }  
    dev.off()     
    corMat <- cbind(corMat, t(apply(abs(corMat), 1, summary)))
    write.csv(corMat, valsName, quote=F, row.names=T)   
  }
}

# Now run through each data frame and produce the values 
valsName <- c('vol', 'cbf', 'gmd', 'ct', 'cortcon', 'reho', 'alff', 'ad', 'rd', 'fa', 'tr')
grepVals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'mprage_jlf_ct_', 'mprage_jlf_cortcon', 'rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_ad_', 'dti_jlf_rd_', 'dti_jlf_fa_', 'dti_jlf_tr_')
for(w in 1:length(valsName)){
  modalName <- valsName[w]
  grepName <- grepVals[w]
  csvName <- paste0(modalName, '.modal.data.age.reg', sep='')
  roughAllFunction(get(csvName), grepName, modalName)
}
for(w in 1:length(valsName)){
  modalName <- valsName[w]
  grepName <- grepVals[w]
  csvName <- paste0(modalName, '.modal.data.age.reg', sep='')
  roughAllFunctionLobe(get(csvName), grepName, paste(modalName, '-Lobe', sep=''))
}
for(w in 1:length(valsName)){
  modalName <- valsName[w]
  grepName <- grepVals[w]
  csvName <- paste0(modalName, '.modal.data.age.reg.mr', sep='')
  modalName <- paste(valsName[w], 'MR', sep='')
  roughAllFunction(get(csvName), grepName, modalName)
}
for(w in 1:length(valsName)){
  modalName <- valsName[w]
  grepName <- grepVals[w]
  csvName <- paste0(modalName, '.modal.data.age.reg.mr', sep='')
  modalName <- paste(valsName[w], 'MR', sep='')
  roughAllFunctionLobe(get(csvName), grepName, paste(modalName, '-Lobe', sep=''))
}

q()
# Now produce total metrics corellations
source('/home/adrose/T1QA/scripts/galton/loadGo1Data.R')
detachAllPackages()
set.seed(16)
load('/home/adrose/qapQA/data/1vs28variableModel.RData')
oneVsTwoModel <- mod8
rm(mod8)

# Now work with the data
## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('caret', 'lme4','BaylorEdPsych', 'mgcv', 'ppcor', 'ggplot2')

## Now create the training data set and create the outcomes for all of the training data sets
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
#folds <- createFolds(raw.lme.data$averageRating.x, k=3, list=T, returnTrain=T)
load('/home/adrose/qapQA/data/foldsToUse.RData')
raw.lme.data[,3:32] <- scale(raw.lme.data[,3:32], center=T, scale=T)
index <- unlist(folds[1])
trainingData <- raw.lme.data#[index,]
validationData <- raw.lme.data#[-index,]

## Now create our outcomes
# First create our zero vs not zero outcome for everyone 
trainingData$variable <- rep('ratingNULL', nrow(trainingData))
# Now lets do our 1 vs 2 model for everyone 
trainingData$oneVsTwoOutcome <- predict(oneVsTwoModel, newdata=trainingData,
					       allow.new.levels=T, type='response')

## Now merge our scaled data values with the original data values 
all.train.data <- merge(mergedQAP, trainingData, by='bblid')

## Now create our age regressed variables 
tmp <- cbind(all.train.data[,grep('mprage_jlf_ct', names(all.train.data))], all.train.data[,grep('mprage_jlf_vol', names(all.train.data))])
# Now trim non cortical regions
tmp <- tmp[,-seq(99,129)]
meanCT <- NULL
for(i in seq(1, nrow(tmp))){
  tmpVal <- weighted.mean(x=tmp[i,1:98], w=tmp[i,99:196])
  meanCT <- append(meanCT, tmpVal)
}
tmp <- read.csv('/home/adrose/qapQA/data/averageGMD.csv')
all.train.data <- merge(all.train.data, tmp, by=c('bblid', 'scanid'))
rm(tmp)
all.train.data$meanVOL <- apply(all.train.data[,2623:2720], 1, sum)
all.train.data$meanVOL2 <- apply(all.train.data[,2623:2720], 1, mean)

# Now create our scaled age and age squared values
all.train.data$age <- scale(all.train.data$ageAtGo1Scan)
all.train.data$ageSq <- (scale(all.train.data$ageAtGo1Scan))^2

# Now produce our age regressed values 
all.train.data$meanCTAgeReg <- lm(meanCT ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanGMDAgeReg <- lm(meanGMD ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanVOLAgeReg <- lm(meanVOL ~ age + ageSq + sex, data=all.train.data)$residuals
all.train.data$meanVOL2AgeReg <- lm(meanVOL2 ~ age + ageSq + sex, data=all.train.data)$residuals

# Now attach and age regress the mean GM CBF calues
cbfDat <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_PcaslQaData_20170403.csv')
all.train.data <- merge(all.train.data, cbfDat, by='bblid')
all.train.data$meanCBFAgeReg <- NA
values <- lm(pcaslMeanGMValue ~ age + ageSq + sex, data=all.train.data)$residuals
index <- as.numeric(names(residuals(lm(pcaslMeanGMValue ~ age + ageSq + sex, data=all.train.data))))
all.train.data$meanCBFAgeReg[index] <- values


# Now produce our cor matrix
tmp <- merge(all.train.data, gmd.modal.data.age.reg, by='bblid')
foo <- createCorRows(tmp[which(tmp$sex.x==1),], 'mean')
bar <- createCorRows(tmp[which(tmp$sex.x==2),], 'mean')
write.csv(foo , 'globalBrainMetrics-gender1.csv', quote=F)
write.csv(bar , 'globalBrainMetrics-gender2.csv', quote=F)

