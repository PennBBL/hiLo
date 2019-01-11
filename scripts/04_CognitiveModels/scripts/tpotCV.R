source('~/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart')

vol.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
alff.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
reho.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
fa.data <- read.csv('~/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFATracts.csv')
colnames(fa.data) <- gsub(x=colnames(fa.data), pattern='jhutract', replacement='jlf')
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))
all.data <- merge(all.data, alff.data)
all.data <- merge(all.data, reho.data)
all.data2 <- merge(vol.data, reho.data)

runTpotOnAll <- function(x, y, nFold=10, grepID){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  trainSeq <- seq(1, length(y))
  modelOut <- matrix(0, dim(x)[2], nFold)
  rownames(modelOut) <- colnames(x)
  # Finally scale our data
  #inputX <- as.matrix(scale(x))
  inputX <- as.matrix(x)
  #inputY <- as.matrix(scale(y))
  inputY <- as.matrix(y)

  # Now we need to loop thorugh each fold and get our output fit stats
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- inputX[index,]
    trainY <- inputY[index]
    testX <- as.matrix(inputX)[-index,]
    for.lm <- data.frame(inputY, inputX)
    colnames(for.lm)[1] <- 'y'

    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0,nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)
    #modelFit <- lm(y~/., data=for.lm)
    modelOut[rownames(modelOut) %in% rownames(coef(modelFit)),i] <- 1

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, data.matrix(testX))
    #outputCvValsR[trainSeq[-index]] <- predict(modelFit, data.frame(testX))
  }
  # Now return the output CvVals
  output <- list()
  output[[1]] <- outputCvValsR
  output[[2]] <- modelOut
  return(output)
}

## Set up a parallel backend
cl <- makeCluster(36)
registerDoParallel(cl)
dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','reho.data', 'alff.data')
outName <- c('Volume', 'CBF', 'GMD', 'MD', 'ReHo', 'ALFF','All')
grepValue <- c(rep('_jlf_', 7), '_jlf_')
allIterations <- matrix(NA, nrow=7, ncol=126)
index <- c(6,21,35,35,21,7,1)
indexF <- c(1,7,22,42,58,64,126)
indexE <- c(6,21,41,56,63,64,127)
row.check <- 1
col.check <- 1
for(i in 1:6){
  to.add <- combn(dataNames,i)
  allIterations[1:row.check,indexF[i]:indexE[i]] <- to.add
  row.check <- row.check+1
  col.check <- col.check+index[i]
}
dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','reho.data', 'alff.data','all.data')
allR <- foreach (q=1:1000, .combine='rbind',.packages=c('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart'),.export=ls(envir=globalenv())) %dopar%{
  outMat <- matrix(NA, ncol=5, nrow=7)
  for(i in 1:length(dataNames)){
    out.data <- NULL
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep("_jlf_", names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    out.data <- cbind(tmpDatY, tmpDatX)
    # Now write the csv for the mbp notebook
    #out.name <- paste("./maleData/", i, "_maleData.csv", sep='')
    #write.csv(out.data, out.name, quote=F, row.names=F)
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    ## Now obtain the cv r-squared
    rss <- sum((predVals[[1]] - tmpDatY) ^ 2)  ## residual sum of squares
    tss <- sum((tmpDatY - mean(tmpDatY)) ^ 2)  ## total sum of squares
    corVal <- 1 - rss/tss
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    outMat[i,] <- outRow
  }
  print(outMat)
}
write.csv(allR, "allIndivRValsMALE.csv", quote=F, row.names=T)
orig <- allR
allR <- as.data.frame(allR)
allR <- dcast(V2 ~ V1, data=allR, value.var='V3')
outMean <- round(apply(allR, 2, mean), digits=2)
outMedian <- round(apply(allR, 2, median), digits=2)
outMin <- round(apply(allR, 2, min), digits=2)
outMax <- round(apply(allR, 2, max), digits=2)
outSD <- round(apply(allR, 2, sd), digits=3)
output <- cbind(orig[1:7,4], orig[1:7,5], outMean[2:8], outMedian[2:8], outMin[2:8], outMax[2:8], outSD[2:8])
write.csv(output, 'tmpAllRValsMaleAR.csv', quote=F, row.names=F)
## Now plot these values
rownames(output) <- NULL
colnames(output) <- c('n','p','Mean','Median','Min','Max','SD')
modal <- outName
output <- cbind(output, modal)
output <- as.data.frame(output)
output$Mean <- as.numeric(as.character(output$Mean))
output$SD <- as.numeric(as.character(output$SD))
#output$modal <- apply(allIterations, 2, function(x) paste(x, collapse='_'))
#output <- output[which(output$Mean>.2),]
outplot <- ggplot(output, aes(x=modal, y=Mean)) +
  geom_bar(stat="identity",position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                 position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0, .23)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold"))
pdf("maleCVRSquaredVals.pdf", width=20, height=12)
print(outplot)
dev.off()  

## Now create a null distribution
allRN <- foreach (q=1:1000, .combine='rbind',.packages=c('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart'),.export=ls(envir=globalenv())) %dopar%{
  outMat <- matrix(NA, ncol=5, nrow=length(dataNames))
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy[sample(nrow(tmpDat))]
    out.data <- cbind(tmpDatY, tmpDatX)
    # Now write the csv for the mbp notebook
    #out.name <- paste("./maleData/", i, "_maleData.csv", sep='')
    #write.csv(out.data, out.name, quote=F, row.names=F)
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    rss <- sum((predVals[[1]] - tmpDatY) ^ 2)  ## residual sum of squares
    tss <- sum((tmpDatY - mean(tmpDatY)) ^ 2)  ## total sum of squares
    corVal <- 1 - rss/tss
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    outMat[i,] <- outRow
  }
  print(outMat)
}
write.csv(allRN, "allIndivRValsMALEPERM.csv", quote=F, row.names=T)
## Now plot the null vs the real ditribution
origN <- allRN
allRN <- as.data.frame(allRN)
allRN <- dcast(V2 ~ V1, data=allRN, value.var='V3')
outMean <- round(apply(allRN, 2, mean), digits=2)
outMedian <- round(apply(allRN, 2, median), digits=2)
outMin <- round(apply(allRN, 2, min), digits=2)
outMax <- round(apply(allRN, 2, max), digits=2)
outSD <- round(apply(allRN, 2, sd), digits=3)
outputN <- cbind(origN[1:7,4], origN[1:7,5], outMean[2:8], outMedian[2:8], outMin[2:8], outMax[2:8], outSD[2:8])

## Create a histogram of null vs real r-squared values for each domain
allR$Outcome <- 'Real'
allRN$Outcome <- 'Fake'
toPlot <- rbind(allR, allRN)
toPlot <- melt(toPlot, id.vars=c('V2', 'Outcome'))
# Now fix the names of the modalities
toPlot$variable <- as.character(toPlot$variable)
## Now also test for differences between real and fake labels
output.t.vals <- NULL
for(i in 1:length(outName)){
  ## Now find and change names of the new variables
  toPlot[which(toPlot$variable==i),'variable'] <- outName[i]
  ## Now perform a t test between the real and fake labels
  out.t.val <- c(outName[i], t.test(x=toPlot[which(toPlot$variable==outName[i] & toPlot$Outcome=='Real'),'value'], y=toPlot[which(toPlot$variable==outName[i] & toPlot$Outcome=='Fake'),'value'],alternative='greater')[c('statistic')])
  output.t.vals <- rbind(output.t.vals, out.t.val)
}

## Now fix the facet order
toPlot$variable <- factor(toPlot$variable, levels=c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "All"))

out.plot.male <- ggplot(toPlot, aes(x=value, group=Outcome, fill=Outcome)) +
  geom_histogram(data=subset(toPlot,Outcome=='Real'), binwidth=.005) +
  geom_histogram(data=subset(toPlot,Outcome=='Fake'), binwidth=.005) +
  theme_bw() +
  facet_grid(variable ~ .) +
  coord_cartesian(ylim=c(0,800),xlim=c(-.1,.25)) +
  ggtitle("Male") +
  xlab(bquote('CV R'^2)) + theme(legend.position="none")
pdf("maleCVRSquaredVals2.pdf", width=8, height=6)
print(out.plot.male)
dev.off()  

# Now do this for females
allR <- foreach (q=1:1000, .combine='rbind',.packages=c('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart'),.export=ls(envir=globalenv())) %dopar%{
  outMat <- matrix(NA, ncol=5, nrow=7)
  for(i in 1:length(dataNames)){
    out.data <- NULL
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==2),]
    tmpDatX <- tmpDat[,grep("_jlf_", names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    out.data <- cbind(tmpDatY, tmpDatX)
    # Now write the csv for the mbp notebook
    #out.name <- paste("./maleData/", i, "_maleData.csv", sep='')
    #write.csv(out.data, out.name, quote=F, row.names=F)
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    rss <- sum((predVals[[1]] - tmpDatY) ^ 2)  ## residual sum of squares
    tss <- sum((tmpDatY - mean(tmpDatY)) ^ 2)  ## total sum of squares
    corVal <- 1 - rss/tss
    #cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    outMat[i,] <- outRow
  }
  print(outMat)
}
write.csv(allR, "allIndivRValsFemale.csv", quote=F, row.names=T)
orig <- allR
allR <- as.data.frame(allR)
allR <- dcast(V2 ~ V1, data=allR, value.var='V3')
outMean <- round(apply(allR, 2, mean), digits=2)
outMedian <- round(apply(allR, 2, median), digits=2)
outMin <- round(apply(allR, 2, min), digits=2)
outMax <- round(apply(allR, 2, max), digits=2)
outSD <- round(apply(allR, 2, sd), digits=3)
output <- cbind(orig[1:7,4], orig[1:7,5], outMean[2:8], outMedian[2:8], outMin[2:8], outMax[2:8], outSD[2:8])
write.csv(output, 'tmpAllRValsFemaleAR.csv', quote=F, row.names=F)
## Now plot these values
rownames(output) <- NULL
colnames(output) <- c('n','p','Mean','Median','Min','Max','SD')
modal <- outName
output <- cbind(output, modal)
output <- as.data.frame(output)
output$Mean <- as.numeric(as.character(output$Mean))
output$SD <- as.numeric(as.character(output$SD))
output$modal <- factor(output$modal, levels=modal[c(1:5,7,8,6)])
outplot <- ggplot(output, aes(x=modal, y=Mean)) +
  geom_bar(stat="identity",position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                 position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0, .25)) 
pdf("femaleCVRSquaredVals.pdf")
print(outplot)
dev.off()  

## Now create a null distribution
allRN <- foreach (q=1:1000, .combine='rbind',.packages=c('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart'),.export=ls(envir=globalenv())) %dopar%{
  outMat <- matrix(NA, ncol=5, nrow=length(dataNames))
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==2),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy[sample(nrow(tmpDat))]
    out.data <- cbind(tmpDatY, tmpDatX)
    # Now write the csv for the mbp notebook
    #out.name <- paste("./femaleData/", i, "_femaleData.csv", sep='')
    #write.csv(out.data, out.name, quote=F, row.names=F)
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    rss <- sum((predVals[[1]] - tmpDatY) ^ 2)  ## residual sum of squares
    tss <- sum((tmpDatY - mean(tmpDatY)) ^ 2)  ## total sum of squares
    corVal <- 1 - rss/tss
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    outMat[i,] <- outRow
  }
  print(outMat)
}

## Now plot the null vs the real ditribution
write.csv(allRN, "allIndivRValsFEMALEPERM.csv", quote=F, row.names=T)
origN <- allRN
allRN <- as.data.frame(allRN)
allRN <- dcast(V2 ~ V1, data=allRN, value.var='V3')
outMean <- round(apply(allRN, 2, mean), digits=2)
outMedian <- round(apply(allRN, 2, median), digits=2)
outMin <- round(apply(allRN, 2, min), digits=2)
outMax <- round(apply(allRN, 2, max), digits=2)
outSD <- round(apply(allRN, 2, sd), digits=3)
outputN <- cbind(origN[1:7,4], origN[1:7,5], outMean[2:8], outMedian[2:8], outMin[2:8], outMax[2:8], outSD[2:8])

## Create a histogram of null vs real r-squared values for each domain
allR$Outcome <- 'Real'
allRN$Outcome <- 'Fake'
toPlot <- rbind(allR, allRN)
toPlot <- melt(toPlot, id.vars=c('V2', 'Outcome'))
# Now fix the names of the modalities
toPlot$variable <- as.character(toPlot$variable)
for(i in 1:length(outName)){
  ## Now find and change names of the new variables
  toPlot[which(toPlot$variable==i),'variable'] <- outName[i]
}

## Now fix the facet order
toPlot$variable <- factor(toPlot$variable, levels=c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "All"))

out.plot.female <- ggplot(toPlot, aes(x=value, group=Outcome, fill=Outcome)) +
  geom_histogram(data=subset(toPlot,Outcome=='Real'), binwidth=.005) +
  geom_histogram(data=subset(toPlot,Outcome=='Fake'), binwidth=.005) +
  theme_bw() +
  facet_grid(variable ~ .)  +
  coord_cartesian(ylim=c(0,800),xlim=c(-.1,.25)) +
  ggtitle("Female") +
  xlab(bquote('CV R'^2)) + theme(legend.position="none") +
  ylab("")

pdf("CVRSquaredVals2.pdf",height=6, width=12)
multiplot(out.plot.male, out.plot.female, cols=2)
dev.off()

## Now kill the cluster
stopCluster(cl)
q()
