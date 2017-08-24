# AFGR August 21 2017

# This script will be used to find a mean z scored importance against the three metrics

# Load library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych', 'ggplot2', 'reshape2', 'caret', 'randomForest', 'parcor')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv') 

# Now declare a function to combine all of the importance outputs
returnAllOut <- function(genderVal, modalityVal, genderName){
  # Declare all of the csv names
  stepN <- paste(genderVal, 'OutSelectVals.csv', sep='')
  stepBeta <- paste(genderVal, 'OutBetaVals.csv', sep='')
  rfImportance <- paste(genderName, 'RFImportance.csv', sep='')
  rfImportance2 <- paste(genderName, 'RFImportance2.csv', sep='')
  lasBeta <- paste('all', genderName, 'Beta.csv', sep='')
  lasSel <- paste('all', genderName, 'Select.csv', sep='')
  nameVals <- c(stepN, stepBeta, rfImportance, rfImportance2, lasBeta, lasSel)
  # Now reach each csv and take out the correct column value
  outData <- matrix(NA, nrow=72, ncol=6)
  tmp <- read.csv(stepN)
  rownames(outData) <- tmp[,1]
  colVal <- c(1,1,1,1,2,2)
  q <- 1
  for( i in nameVals){
    tmp <- read.csv(i)
    colnames(tmp) <- tolower(colnames(tmp))
    modVal <- grep(modalityVal, colnames(tmp))
    outCol <- tmp[match(rownames(outData), tmp[,colVal[q]]),modVal[1]]
    outData[,q] <- outCol
    q <- q + 1
  }
  # Now find any NA mistakes by finding the modeal value across the row
  # If it is NA then set the entire row to NA
  for(i in seq(1,72)){
    modeVal <- Mode(outData[i,])
    if(is.na(modeVal)){
      outData[i,] <-rep(NA, 6) 
    }
  }
  colnames(outData) <- c('AustinN', 'AllBeta', 'RfMSE', 'RfPurity', 'LassoBeta', 'LassoN')
  outData[,2] <- abs(outData[,2])
  outData[,5] <- abs(outData[,5])
  outData <- scale(outData)[1:72,]
  return(outData)
}

returnAllOutMR <- function(genderVal, modalityVal, genderName){
  # Declare all of the csv names
  stepN <- paste(genderVal, 'OutSelectVals.csv', sep='')
  stepBeta <- paste(genderVal, 'OutBetaVals.csv', sep='')
  rfImportance <- paste(genderName, 'RFImportanceModalReg.csv', sep='')
  rfImportance2 <- paste(genderName, 'RFImportanceModalReg2.csv', sep='')
  lasBeta <- paste('all', genderName, 'BetaMR.csv', sep='')
  lasSel <- paste('all', genderName, 'SelectNMR.csv', sep='')
  nameVals <- c(stepN, stepBeta, rfImportance, rfImportance2, lasBeta, lasSel)
  # Now reach each csv and take out the correct column value
  outData <- matrix(NA, nrow=72, ncol=6)
  tmp <- read.csv(stepN)
  rownames(outData) <- tmp[,1]
  checkVals <- c(0,1,0,0,0,0)
  colVal <- c(1,1,1,1,2,2)
  q <- 1
  for( i in nameVals){
    tmp <- read.csv(i)
    colnames(tmp) <- tolower(colnames(tmp))
    modVal <- grep(modalityVal, colnames(tmp))
    if(checkVals[q] == 1){
      if(length(modVal) > 1){
        modVal <- modVal[2]
      }
    }
    outCol <- tmp[match(rownames(outData), tmp[,colVal[q]]),modVal[1]]
    outData[,q] <- outCol
    q <- q + 1
  }
  # Now find any NA mistakes by finding the modeal value across the row
  # If it is NA then set the entire row to NA
  for(i in seq(1,72)){
    modeVal <- Mode(outData[i,])
    if(is.na(modeVal)){
      outData[i,] <-rep(NA, 6) 
    }
  }
  colnames(outData) <- c('AustinN', 'AllBeta', 'RfMSE', 'RfPurity', 'LassoBeta', 'LassoN')
  outData[,2] <- abs(outData[,2])
  outData[,5] <- abs(outData[,5])
  outData <- scale(outData)[1:72,]
  return(outData)
}

# Now create a rough function to create violin plots from the z score data
playTheViolin <- function(zScoreVals, xValue){
  plotData <- melt(zScoreVals)
  plotData[plotData=='NaN'] <- NA
  plotData <- plotData[complete.cases(plotData),]
  # Now plot the data
  outPlot <- ggplot(plotData, aes(x=Var1, y=value)) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=23) + 
    stat_summary(fun.y=median, geom="point", size=2, color="red") + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="z score", x=xValue)
  return(outPlot)
}

playTheViola <- function(zScoreVals, xValue){
  plotData <- zScoreVals
  plotData[plotData=='NaN'] <- NA
  plotData <- plotData[complete.cases(plotData),]
  # Now plot the data
  outPlot <- ggplot(plotData, aes(x=Var1, y=value)) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=23) + 
    stat_summary(fun.y=median, geom="point", size=2, color="red") + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="z score", x=xValue)
  return(outPlot)
}

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
for(z in seq(80, 100, 1)){
  dir.create(paste(baseDir, z, sep=''))
  setwd(paste(baseDir, z, sep=''))
  system("Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/stepImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/mylarsImportance.R & Rscript /home/adrose/hiLo/scripts/04_CognitiveModels/scripts/runRandomForest.R")
}

# Now go across all of the 100 directories and grab the z score values
for(g in 1:2){
  pdfName <- paste(g, '100Fold.pdf', sep='')
  pdf(pdfName)
  for(m in modalityValues){
    print(m)
    valsOut <- NULL
    for(z in seq(1, 100, 1)){
      setwd(paste(baseDir, z, sep=''))
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
  pdf(pdfName)
  for(m in modalityValues){
    valsOut <- NULL
    for(z in seq(1, 100, 1)){
      setwd(paste(baseDir, z, sep=''))
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
