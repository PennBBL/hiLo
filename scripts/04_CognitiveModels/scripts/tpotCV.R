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
all.data <- merge(all.data, fa.data)
all.data <- merge(all.data, alff.data)
all.data <- merge(all.data, reho.data)

runTpotOnAll <- function(x, y, nFold=10, grepID){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  trainSeq <- seq(1, length(y))
  modelOut <- matrix(0, dim(x)[2], nFold)
  rownames(modelOut) <- colnames(x)
  # FInally scale our data
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

dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','fa.data','all.data', 'reho.data', 'alff.data')
#dataNames <- c('fa.data', 'all.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'fa', 'all.data', 'reho', 'alff')
#outName <- c('fa', 'all.data')
grepValue <- c(rep('_jlf_', 7), '_jlf_')
#grepValue <- c('_jlf_', '_jlf_')
allR <- NULL
for(q in seq(1,25)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    out.data <- cbind(tmpDatY, tmpDatX)
    # Now write the csv for the mbp notebook
    out.name <- paste("./maleData/", i, "_maleData.csv", sep='')
    write.csv(out.data, out.name, quote=F, row.names=F)
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    corVal <- cor(predVals[[1]], tmpDatY)^2
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}
orig <- allR
allR <- as.data.frame(allR)
allR <- dcast(V2 ~ V1, data=allR, value.var='V3')
outMean <- round(apply(allR, 2, mean), digits=2)
outMedian <- round(apply(allR, 2, median), digits=2)
outMin <- round(apply(allR, 2, min), digits=2)
outMax <- round(apply(allR, 2, max), digits=2)
outSD <- round(apply(allR, 2, sd), digits=3)
output <- cbind(orig[1:8,4], orig[1:8,5], outMean[2:9], outMedian[2:9], outMin[2:9], outMax[2:9], outSD[2:9])
write.csv(output, 'tmpAllRValsMaleAR.csv', quote=F, row.names=F)
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
pdf("maleCVRSquaredVals.pdf")
print(outplot)
dev.off()  

# Now do this for females
allR <- NULL
for(q in seq(1,10)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==2),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    out.data <- cbind(tmpDatY, tmpDatX)
    # Now write the csv for the mbp notebook
    out.name <- paste("./femaleData/", i, "_femaleData.csv", sep='')
    write.csv(out.data, out.name, quote=F, row.names=F)
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10, grepValue[i])
    corVal <- cor(predVals[[1]], tmpDatY)^2
    cvICC <- ICC(cbind(predVals[[1]], tmpDatY))$results[4,2]
    cvRMSE <- sqrt(mean((tmpDatY-predVals[[1]])^2))
    outRow <- c(i, q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}
orig <- allR
allR <- as.data.frame(allR)
allR <- dcast(V2 ~ V1, data=allR, value.var='V3')
outMean <- round(apply(allR, 2, mean), digits=2)
outMedian <- round(apply(allR, 2, median), digits=2)
outMin <- round(apply(allR, 2, min), digits=2)
outMax <- round(apply(allR, 2, max), digits=2)
outSD <- round(apply(allR, 2, sd), digits=2)
output <- cbind(orig[1:8,4], orig[1:8,5], outMean[2:9], outMedian[2:9], outMin[2:9], outMax[2:9], outSD[2:9])
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

## Down here produce the beta weights from one final model for all w/in and across ridge reg models
pdf('stirImport.pdf', height=20, width=60)
for(i in 1:length(dataNames)){
  tmpDat <- get(dataNames[i])
  tmpDat <- tmpDat[which(tmpDat$sex==1),]
  tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
  tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
  fit.cv <- cv.glmnet(x=as.matrix(tmpDatX), y=as.matrix(tmpDatY), alpha=1,nfolds=10)
  lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
  modelFit <- glmnet(x=as.matrix(tmpDatX), y=as.matrix(tmpDatY), alpha=1, lambda=lambdaVal)
  ## Now run relief F on these guys
  tmpDatY2<- tmpDatY
  tmpDatY2[tmpDatY2>0.2397024] <- 1
  tmpDatY2[tmpDatY2<=0.2397024] <- 0
  neighbor.idx.observed <- find.neighbors(tmpDatX, tmpDatY2, k = 0, method = RF.method)
  results.list <- stir(tmpDatX, neighbor.idx.observed, k = k, metric = metric, method = RF.method)
  t_sorted_multisurf <- results.list$STIR_T
  t_sorted_multisurf$attribute <- rownames(t_sorted_multisurf)
  toPlot <- data.frame(cbind(t_sorted_multisurf$attribute, as.numeric(as.character(t_sorted_multisurf$t.stat))))
  toPlot[,1] <- factor(toPlot[,1], levels=t_sorted_multisurf$attribute)
  ## Now plot the importance metrics
  out.img <- ggplot(toPlot, aes(x=toPlot[,1], y=as.numeric(as.character(toPlot[,2])))) +
    geom_point() +
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15))
  print(out.img)
}
dev.off()
