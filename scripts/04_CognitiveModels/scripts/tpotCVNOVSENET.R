source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet','psych','reshape2', 'caret','MASS', 'methods', 'ggplot2', 'rpart', 'e1071')

vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
cbf.data$pcasl_jlf_cbf_MeanGM <- cbf.data$pcaslMeanGMValue
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ccData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')
fa.data.wm <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFATractsData.csv')
tr.data$dti_jlf_tr_MeanTR <- apply(tr.data[,grep('dti_jlf_tr_', names(tr.data))], 1, mean)
factorScores <- read.csv('/home/adrose/Brain_Factor_Scores_Volume_GMD_Cortical-Thickness.csv')
fac.scores <- merge(factorScores, vol.data)
factorScoresPerf <- read.csv('/home/adrose/Brain_Factor_Scores_CBF.csv')
fac.scores.perf <- merge(factorScoresPerf, vol.data)
factorScoresFunc <- read.csv('/home/adrose/brain_factor_scores_reho_alff.csv')
fac.scores.func <- merge(factorScoresFunc, vol.data)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm),
          median = median(xx[[col]], na.rm=na.rm),
          min  = min    (xx[[col]], na.rm=na.rm),
          max  = max    (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)))
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)))
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)))
all.fact <- merge(fac.scores, fac.scores.func)
all.fact <- merge(all.fact, fac.scores.perf)
all.fact <- all.fact[,-seq(8,16)]
all.fact <- all.fact[,-(grep('mprage_jlf_vol_', names(all.fact)))]

runTpotOnAll <- function(x, y, nFold=10){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  outputCvValsL <- rep(NA, length(y))
  trainSeq <- seq(1, length(y))
  # Now we need to loop thorugh each fold and get our output fit stats
  #cl <- makeCluster(20)
  #registerDoParallel(cl)
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- as.matrix(x)[index,]
    trainY <- as.vector(y)[index]
    testX <- as.matrix(x)[-index,]

    # Run variable selection
    varSelectDF <- as.data.frame(cbind(trainY, trainX))
    #varSelectDF$sex <- 1
    colnames(varSelectDF)[1] <- 'F1_Exec_Comp_Cog_Accuracy'
    #selectSums <- returnSelectionN(dataFrame=varSelectDF, grepID='_jlf_', genderID=1, iterationCount=100, nCor=31)
    #valsToUse <- which(as.numeric(returnSelectionCol(selectSums)[,2]) > 24)
    #nameVals <- rownames(selectSums)[valsToUse]
    #colVals <- which(colnames(trainX) %in% nameVals)
    
    # Now apply the selection
    #trainX <- trainX[,colVals]
    #testX <- testX[,colVals]  
    #pVal <- dim(trainX)[2]
    #varSelectDF <- varSelectDF[,c(1, which(names(varSelectDF) %in% colnames(trainX)))]

    # Build a regression tree
    #tc <- trainControl("cv", number=4)
    #Grid <- expand.grid(mtry = seq(4,16,4))
    #colNamesFreeze <- colnames(varSelectDF)[-1]
    #tree1 <- rpart(F1_Exec_Comp_Cog_Accuracy~.,data=varSelectDF, method='anova',control=rpart.control(minsplit=10,maxdepth=1))
    #trainX <- cbind(trainX, predict(tree1))
    #newPredVals <- as.data.frame(testX)
    #colnames(newPredVals) <- colNamesFreeze
    #testX <- cbind(testX, predict(tree1, newdata=newPredVals))

    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0,nfolds=10)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal)

    # Now build a LM model
    tmpDataFrame <- as.data.frame(cbind(trainY, trainX))
    #obj <- tune(svm, trainY~., data=tmpDataFrame, kernel='linear', ranges=list(cost=10^(-2:2), gamma=2^(-2:2)),
    #          tunecontrol = tune.control(sampling = "fix"))
    #modelLMFit <- svm(trainY~., data=tmpDataFrame, epsilon=0.001, tolerence=0.0001, kernel='linear', cost=1)
    #lmPredVals <- as.data.frame(testX)
    #colnames(lmPredVals) <- colnames(tmpDataFrame)[-1]

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, testX)
    #outputCvValsL[trainSeq[-index]] <- predict(modelLMFit, lmPredVals)
  }
  #stopCluster(cl)
  # Now return the output CvVals
  #output <- list()
  output <- outputCvValsR
  #output[[2]] <- outputCvValsL
  return(output)
}

dataNames <- c('vol.data','cbf.data','gmd.data','tr.data','all.data')
outName <- c('vol', 'cbf', 'gmd', 'tr', 'all.dat')
grepValue <- c(rep('_jlf_', 5))
allR <- NULL
for(q in seq(1,100)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==1),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10)
    corVal <- cor(predVals, tmpDatY)
    outRow <- c(outName[i], q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}
rownames(allR) <- NULL
allRVals <- as.data.frame(allR)
allRVals$V3 <- as.numeric(as.character(allRVals$V3))^2
foo <- summarySE(allRVals, measurevar='V3', groupvars='V1')
foo <- foo[order(foo$V3, decreasing=T),]
allRVals$V1 <- factor(allRVals$V1, levels=foo$V1)
outPlot <- ggplot(allRVals, aes(x=V1, y=as.numeric(as.character(V3)))) + 
  geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + 
  labs(title='CV Prediction by modality', y='CV R-squared', x='Modality')

foo$n <- c(436, 603, 442, 603, 574)
foo$p <- c(267, 73, 68, 62, 64)
# Now create a summary table which includes mean, median, min, and max values for each modality
write.csv(foo, 'maleSummaryMetrics.csv', quote=F, row.names=F)


pdf('image.pdf')
outPlot
dev.off()

# Now do females!
allR <- NULL
for(q in seq(1,100)){
  for(i in 1:length(dataNames)){
    tmpDat <- get(dataNames[i])
    tmpDat <- tmpDat[which(tmpDat$sex==2),]
    tmpDatX <- tmpDat[,grep(grepValue[i], names(tmpDat))]
    tmpDatY <- tmpDat$F1_Exec_Comp_Cog_Accuracy
    predVals <- runTpotOnAll(tmpDatX, tmpDatY, 10)
    corVal <- cor(predVals, tmpDatY)
    outRow <- c(outName[i], q, corVal, dim(tmpDatX)[1], dim(tmpDatX)[2])
    allR <- rbind(allR, outRow)
    #write.csv(allR, 'tmpAllRValsNOVS2.csv', quote=F, row.names=F)
  }
  print(q)
}

rownames(allR) <- NULL
allRVals <- as.data.frame(allR)
allRVals$V3 <- as.numeric(as.character(allRVals$V3))^2
foo <- summarySE(allRVals, measurevar='V3', groupvars='V1')
foo <- foo[order(foo$V3, decreasing=T),]
allRVals$V1 <- factor(allRVals$V1, levels=foo$V1)
outPlot <- ggplot(allRVals, aes(x=V1, y=as.numeric(as.character(V3)))) + 
  geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + 
  labs(title='CV Prediction by modality', y='CV R-squared', x='Modality')

foo$p <- c(267, 73, 68, 62, 64)
foo$n <- c(677, 543, 677, 550, 649)
# Now create a summary table which includes mean, median, min, and max values for each modality
write.csv(foo, 'femaleSummaryMetrics.csv', quote=F, row.names=F)
q()




cost <- 2^(1:4)
gamma <- 2^(-1:1)
epsilon <- seq(.001, .1, .001)
parms <- expand.grid(cost = cost, gamma = gamma, epsilon = epsilon)
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(df2$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    deve <- df2[df2$fold != j, ]
    test <- df2[df2$fold == j, ]
    mdl <- e1071::svm(fml, data = deve, type = "C-classification", kernel = "radial", cost = c, gamma = g, probability = TRUE)
    pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
    data.frame(y = test$DEFAULT, prob = attributes(pred)$probabilities[, 2])
  }
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob) 
  data.frame(parms[i, ], roc = roc$auc[1])
}
