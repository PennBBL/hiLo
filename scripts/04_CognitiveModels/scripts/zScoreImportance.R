# AFGR August 21 2017

# This script will be used to find a mean z scored importance against the three metrics
# Load library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'parcor')

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

# Now repeat this procedure 100 times so we can get more spread amongst our violin plots
baseDir <- "/home/adrose/hiLo/scripts/04_CognitiveModels/scripts/computeZScore/"
for(z in seq(1, 100, 1)){
  dir.create(paste(baseDir, z, sep=''))
  setwd(paste(baseDir, z, sep=''))
  system("Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/stepImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/mylarsImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/runRandomForest.R")
}

# Now go across all of the 100 directories and grab the z score values
for(g in 1:2){
  pdfName <- paste(g, '100Fold.pdf', sep='')
  pdf(pdfName, width=20, height=16)
  for(m in modalityValues){
    print(m)
    valsOut <- NULL
    for(z in seq(1, 100, 1)){
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
  pdfName <- paste(g, '100FoldMR.pdf', sep='')
  pdf(pdfName, width=20, height=16)
  for(m in modalityValues){
    valsOut <- NULL
    for(z in seq(1, 100, 1)){
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
