# AFGR August 21 2017

# This script will be used to find a mean z scored importance against the three metrics
# Load library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'parcor')

# Load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')

# First run all of our importance scripts to make sure we have the proper values to z score
system("Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/stepImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/mylarsImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/runRandomForest.R")
# Now loop through each of the values and create the violin plot
genderNames <- c('male', 'female')
modalityValues <- c('vol', 'cbf', 'gmd', 'tr')
for(w in seq(1,2,1)){
  outpdf <- paste(w, 'pdfName.pdf', sep='')
  pdf(outpdf)
  for(m in modalityValues){
    zSco <- returnAllOut(w, m, genderNames[w])    
    print(playTheViolin(zSco, m))
    outCsvName <- paste(w,m,'outZScores.csv', sep='')
    meanValue <- apply(zSco, 1, function(x) mean(x, na.rm=T))
    zSco <- cbind(zSco, meanValue)
    write.csv(zSco, outCsvName, quote=F, row.names=T)
  }
  dev.off()
}
# Now do modality regressed 
for(w in seq(1,2,1)){
  outpdf <- paste(w, 'pdfNameMR.pdf', sep='')
  pdf(outpdf)
  for(m in modalityValues){
    zSco <- returnAllOutMR(w, m, genderNames[w])    
    print(playTheViolin(zSco, m))
    outCsvName <- paste(w,m,'outZScoresMR.csv', sep='')
    meanValue <- apply(zSco, 1, function(x) mean(x, na.rm=T))
    zSco <- cbind(zSco, meanValue)
    write.csv(zSco, outCsvName, quote=F, row.names=T)
  }
  dev.off()
}

# Now I need to make a model with the top 5, 15, and 30 variables and return fit stats 
grepNames <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')
output <- NA
for(w in seq(1,2,1)){
  q <- 1 
  for(m in modalityValues){
    # Get the csv name 
    inCsv <-  paste(w,m,'outZScoresMR.csv', sep='')
    vals <- read.csv(inCsv)
    vals <- vals[order(vals$meanValue, decreasing=T),]
    appendValue <- grepNames[q]
    allOut <- NA    
    # Now build the model based on the output values 
    for(v in c(5,15,30)){
      roiNames <- as.character(vals[1:v,1])
      roiNames <- paste(appendValue, roiNames, sep='')
      tmpVals <- get(paste(m, '.data', sep=''))
      tmpVals <- tmpVals[which(tmpVals$sex==w),]
      roiVals <- tmpVals[roiNames]
      roiVals <- cbind(tmpVals$F1_Exec_Comp_Cog_Accuracy, roiVals)
      colnames(roiVals)[1] <- 'F1_Exec_Comp_Cog_Accuracy'
      modM <- lm(F1_Exec_Comp_Cog_Accuracy ~ ., data=roiVals)
      outVals <- returnFitMetricsFromModel(modM)
      outVals <- cbind(w, m, outVals)
      allOut <- rbind(allOut, outVals)
    }
    q <- q + 1
    output <- rbind(output, allOut)
  }
}

write.csv(output, 'fitMetrics.csv', quote=F, row.names=F)

# Now repeat this procedure 100 times so we can get more spread amongst our violin plots
baseDir <- "/home/adrose/hiLo/scripts/04_CognitiveModels/scripts/computeZScore/All"
for(z in seq(1, 25, 1)){
  dir.create(paste(baseDir, z, sep=''))
  setwd(paste(baseDir, z, sep=''))
  system("Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/stepImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/mylarsImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/runRandomForest.R")
}

system("sleep 900")
# Now go across all of the 100 directories and grab the z score values
for(g in 1:2){
  pdfName <- paste(g, '-25Fold.pdf', sep='')
  pdf(pdfName, width=20, height=16)
  for(m in modalityValues){
    print(m)
    valsOut <- NULL
    for(z in seq(1, 25, 1)){
      vals <- returnAllOut(g, m, genderNames[g])
      vals <- melt(vals)
      valsOut <- rbind(valsOut, vals)
      print(m)
    }
    out <- playTheViola(valsOut, m)
    print(out)
  }
  dev.off()
}

for(g in 1:2){
  pdfName <- paste(g, '-25FoldMR.pdf', sep='')
  pdf(pdfName, width=20, height=16)
  for(m in modalityValues){
    valsOut <- NULL
    for(z in seq(1, 25, 1)){
      vals <- returnAllOutMR(g, m, genderNames[g])
      vals <- melt(vals)
      valsOut <- rbind(valsOut, vals)
      print(m)
    }
    out <- playTheViola(valsOut, m)
    print(out)
  }
  dev.off()
}
