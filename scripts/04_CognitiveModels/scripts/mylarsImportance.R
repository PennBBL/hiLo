# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('methods', 'glmnet', 'bootstrap', 'psych','reshape2', 'caret','parcor','foreach', 'doParallel')

# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv') 

#vol.data <- returnPercentileGroup('me', vol.data$F1_Exec_Comp_Cog_Accuracy, vol.data)
#cbf.data <- returnPercentileGroup('me', cbf.data$F1_Exec_Comp_Cog_Accuracy, cbf.data)
#gmd.data <- returnPercentileGroup('me', gmd.data$F1_Exec_Comp_Cog_Accuracy, gmd.data)
#tr.data <- returnPercentileGroup('me', tr.data$F1_Exec_Comp_Cog_Accuracy, tr.data)

# Load any functions
runMyLars <- function(x , y, nIters=101, nCor=NA, alphaSequence=NA){
  outputMatrix <- matrix(0, ncol(x), nIters+1)
  outputMatrix[,1] <- colnames(x)
  for(i in 2:nIters+1){
    tmpVals <- mylars(X=x, y=y, k=10)$coefficients
    outputMatrix[,i] <- tmpVals
  }
  return(outputMatrix)
}

mylars100 <- function(x, y, nIters=100){
  outputVal <- mylars(x,y)
  # Now set up our parallel environment
  cl <- makeCluster(5)
  registerDoParallel(cl)
  # Now run through 100 loops and return the output vals from bootstrapped 
  # runs of mylars
  outMeanBetaVals <- foreach(i=seq(1,nIters), .combine='cbind') %dopar% {
      # First load required library(s)
      source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
      source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
      install_load('caret', 'parcor')
      
      # Create some static data points
      dataToUse <- x
      dataToUse$y <- y

      # create our model
      index <- createDataPartition(y,list=F)
      tmpY <- y[index]
      tmpX <- x[index,]
      modOut <-  mylars(tmpX, tmpY)
      
      # return the output
      coefficients(modOut)
  }
  stopCluster(cl)
  outBeta <- apply(outMeanBetaVals, 1, mean)
  outputVal$coefficients <- outBeta
  return(outputVal)
}

# Now first report the number of times each thing variable is selected
male.vol.data <- vol.data[which(vol.data$sex==1),]
vol.col <- grep('mprage_jlf_vol', names(vol.data))
male.vol.values <- scale(male.vol.data[,vol.col])[,1:length(vol.col)]
male.vol.outcome <- scale(male.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.vol.outcome <- male.vol.outcome[complete.cases(male.vol.data[,vol.col])]
male.vol.values <- male.vol.values[complete.cases(male.vol.data[,vol.col]),]
male.vol.selection <- runMyLars(male.vol.values, male.vol.outcome, nCor=30, alphaSequence=1)
male.vol.sum <- as.matrix(returnSelectionCol(male.vol.selection))
male.vol.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.vol.values, male.vol.outcome)$coefficients)))
rownames(male.vol.enet.beta) <- colnames(male.vol.values)
rownames(male.vol.sum) <- colnames(male.vol.values)
male.vol.out <- cbind(male.vol.enet.beta, male.vol.sum)

# Now do female data 
female.vol.data <- vol.data[which(vol.data$sex==2),]
female.vol.values <- scale(female.vol.data[,vol.col])[,1:length(vol.col)]
female.vol.outcome <- scale(female.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.vol.outcome <- female.vol.outcome[complete.cases(female.vol.data[,vol.col])]
female.vol.values <- female.vol.values[complete.cases(female.vol.data[,vol.col]),]
female.vol.selection <- runMyLars(female.vol.values, female.vol.outcome, nCor=30, alphaSequence=1)
female.vol.sum <- as.matrix(returnSelectionCol(female.vol.selection))
female.vol.lambda <- tuneAlpha(x=female.vol.values, y=female.vol.outcome, alphaSequence=1, nFolds=10)[2]
female.vol.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.vol.values, female.vol.outcome)$coefficients)))
rownames(female.vol.enet.beta) <- colnames(female.vol.values)
rownames(female.vol.sum) <- colnames(female.vol.values)
female.vol.out <- cbind(female.vol.enet.beta, female.vol.sum)

# Now do CBF
male.cbf.data <- cbf.data[which(cbf.data$sex==1),]
cbf.col <- grep('pcasl_jlf_cbf', names(cbf.data))
male.cbf.values <- scale(male.cbf.data[,cbf.col])[,1:length(cbf.col)]
male.cbf.outcome <- scale(male.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.cbf.outcome <- male.cbf.outcome[complete.cases(male.cbf.data[,cbf.col])]
male.cbf.values <- male.cbf.values[complete.cases(male.cbf.data[,cbf.col]),]
male.cbf.selection <- runMyLars(male.cbf.values, male.cbf.outcome, nCor=30, alphaSequence=1)
male.cbf.sum <- as.matrix(returnSelectionCol(male.cbf.selection))
male.cbf.lambda <- tuneAlpha(x=male.cbf.values, y=male.cbf.outcome, alphaSequence=1, nFolds=10)[2]
male.cbf.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.cbf.values, male.cbf.outcome)$coefficients)))
rownames(male.cbf.enet.beta) <- colnames(male.cbf.values)
rownames(male.cbf.sum) <- colnames(male.cbf.values)
male.cbf.out <- cbind(male.cbf.enet.beta, male.cbf.sum)

# Now do female data 
female.cbf.data <- cbf.data[which(cbf.data$sex==2),]
female.cbf.values <- scale(female.cbf.data[,cbf.col])[,1:length(cbf.col)]
female.cbf.outcome <- scale(female.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.cbf.outcome <- female.cbf.outcome[complete.cases(female.cbf.data[,cbf.col])]
female.cbf.values <- female.cbf.values[complete.cases(female.cbf.data[,cbf.col]),]
female.cbf.selection <- runMyLars(female.cbf.values, female.cbf.outcome, nCor=30, alphaSequence=1)
female.cbf.sum <- as.matrix(returnSelectionCol(female.cbf.selection))
female.cbf.lambda <- tuneAlpha(x=female.cbf.values, y=female.cbf.outcome, alphaSequence=1, nFolds=10)[2]
female.cbf.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.cbf.values, female.cbf.outcome)$coefficients)))
rownames(female.cbf.enet.beta) <- colnames(female.cbf.values)
rownames(female.cbf.sum) <- colnames(female.cbf.values)
female.cbf.out <- cbind(female.cbf.enet.beta, female.cbf.sum)

# Now onto GMD
male.gmd.data <- gmd.data[which(gmd.data$sex==1),]
gmd.col <- grep('mprage_jlf_gmd', names(gmd.data))
male.gmd.values <- scale(male.gmd.data[,gmd.col])[,1:length(gmd.col)]
male.gmd.outcome <- scale(male.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.gmd.outcome <- male.gmd.outcome[complete.cases(male.gmd.data[,gmd.col])]
male.gmd.values <- male.gmd.values[complete.cases(male.gmd.data[,gmd.col]),]
male.gmd.selection <- runMyLars(male.gmd.values, male.gmd.outcome, nCor=30, alphaSequence=1)
male.gmd.sum <- as.matrix(returnSelectionCol(male.gmd.selection))
male.gmd.lambda <- tuneAlpha(x=male.gmd.values, y=male.gmd.outcome, alphaSequence=1, nFolds=10)[2]
male.gmd.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.gmd.values, male.gmd.outcome)$coefficients)))
rownames(male.gmd.enet.beta) <- colnames(male.gmd.values)
rownames(male.gmd.sum) <- colnames(male.gmd.values)
male.gmd.out <- cbind(male.gmd.enet.beta, male.gmd.sum)

# Now do female data 
female.gmd.data <- gmd.data[which(gmd.data$sex==2),]
female.gmd.values <- scale(female.gmd.data[,gmd.col])[,1:length(gmd.col)]
female.gmd.outcome <- scale(female.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.gmd.outcome <- female.gmd.outcome[complete.cases(female.gmd.data[,gmd.col])]
female.gmd.values <- female.gmd.values[complete.cases(female.gmd.data[,gmd.col]),]
female.gmd.selection <- runMyLars(female.gmd.values, female.gmd.outcome, nCor=30, alphaSequence=1)
female.gmd.sum <- as.matrix(returnSelectionCol(female.gmd.selection))
female.gmd.lambda <- tuneAlpha(x=female.gmd.values, y=female.gmd.outcome, alphaSequence=1, nFolds=10)[2]
female.gmd.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.gmd.values, female.gmd.outcome)$coefficients)))
rownames(female.gmd.enet.beta) <- colnames(female.gmd.values)
rownames(female.gmd.sum) <- colnames(female.gmd.values)
female.gmd.out <- cbind(female.gmd.enet.beta, female.gmd.sum)

# Now onto trace data
male.tr.data <- tr.data[which(tr.data$sex==1),]
tr.col <- grep('dti_jlf_tr', names(tr.data))
male.tr.values <- scale(male.tr.data[,tr.col])[,1:length(tr.col)]
male.tr.outcome <- scale(male.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.tr.outcome <- male.tr.outcome[complete.cases(male.tr.data[,tr.col])]
male.tr.values <- male.tr.values[complete.cases(male.tr.data[,tr.col]),]
male.tr.selection <- runMyLars(male.tr.values, male.tr.outcome, nCor=30, alphaSequence=1)
male.tr.sum <- as.matrix(returnSelectionCol(male.tr.selection))
male.tr.lambda <- tuneAlpha(x=male.tr.values, y=male.tr.outcome, alphaSequence=1, nFolds=10)[2]
male.tr.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.tr.values, male.tr.outcome)$coefficients)))
rownames(male.tr.enet.beta) <- colnames(male.tr.values)
rownames(male.tr.sum) <- colnames(male.tr.values)
male.tr.out <- cbind(male.tr.enet.beta, male.tr.sum)

# Now do female data 
female.tr.data <- tr.data[which(tr.data$sex==2),]
female.tr.values <- scale(female.tr.data[,tr.col])[,1:length(tr.col)]
female.tr.outcome <- scale(female.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.tr.outcome <- female.tr.outcome[complete.cases(female.tr.data[,tr.col])]
female.tr.values <- female.tr.values[complete.cases(female.tr.data[,tr.col]),]
female.tr.selection <- runMyLars(female.tr.values, female.tr.outcome, nCor=30, alphaSequence=1)
female.tr.sum <- as.matrix(returnSelectionCol(female.tr.selection))
female.tr.lambda <- tuneAlpha(x=female.tr.values, y=female.tr.outcome, alphaSequence=1, nFolds=10)[2]
female.tr.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.tr.values, female.tr.outcome)$coefficients)))
rownames(female.tr.enet.beta) <- colnames(female.tr.values)
rownames(female.tr.sum) <- colnames(female.tr.values)
female.tr.out <- cbind(female.tr.enet.beta, female.tr.sum)

# Now create the output
eNetBetas <- NULL
eNetBetas <- c(male.vol.out[,1], male.cbf.out[,1], male.gmd.out[,1], male.tr.out[,1])
tmp <- c(rep('Vol', length(male.vol.out[,1])), rep('CBF', length(male.cbf.out[,1])), rep('GMD', length(male.gmd.out[,1])), rep('TR', length(male.tr.out[,1])))
rowNames <- c(rownames(male.vol.out), rownames(male.cbf.out), rownames(male.gmd.out), rownames(male.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allmaleBeta.csv', quote=F)
maleBeta <- output

eNetBetas <- NULL
eNetBetas <- c(female.vol.out[,1], female.cbf.out[,1], female.gmd.out[,1], female.tr.out[,1])
tmp <- c(rep('Vol', length(female.vol.out[,1])), rep('CBF', length(female.cbf.out[,1])), rep('GMD', length(female.gmd.out[,1])), rep('TR', length(female.tr.out[,1])))
rowNames <- c(rownames(female.vol.out), rownames(female.cbf.out), rownames(female.gmd.out), rownames(female.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allfemaleBeta.csv', quote=F)
femaleBeta <- output

eNetBetas <- NULL
eNetBetas <- c(male.vol.out[,2], male.cbf.out[,2], male.gmd.out[,2], male.tr.out[,2])
tmp <- c(rep('Vol', length(male.vol.out[,1])), rep('CBF', length(male.cbf.out[,1])), rep('GMD', length(male.gmd.out[,1])), rep('TR', length(male.tr.out[,1])))
rowNames <- c(rownames(male.vol.out), rownames(male.cbf.out), rownames(male.gmd.out), rownames(male.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allmaleSelect.csv', quote=F)
maleSelect <- output

eNetBetas <- NULL
eNetBetas <- c(female.vol.out[,2], female.cbf.out[,2], female.gmd.out[,2], female.tr.out[,2])
tmp <- c(rep('Vol', length(female.vol.out[,1])), rep('CBF', length(female.cbf.out[,1])), rep('GMD', length(female.gmd.out[,1])), rep('TR', length(female.tr.out[,1])))
rowNames <- c(rownames(female.vol.out), rownames(female.cbf.out), rownames(female.gmd.out), rownames(female.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allfemaleSelect.csv', quote=F)
femaleSelect <- output

## Do everything the same but with modality regressed data down here ##
# Now we need to load the data 
# Now we need to load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/gmdData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalReg/jlfTRData.csv')

vol.data <- returnPercentileGroup('me', vol.data$F1_Exec_Comp_Cog_Accuracy, vol.data)
cbf.data <- returnPercentileGroup('me', cbf.data$F1_Exec_Comp_Cog_Accuracy, cbf.data)
gmd.data <- returnPercentileGroup('me', gmd.data$F1_Exec_Comp_Cog_Accuracy, gmd.data)
tr.data <- returnPercentileGroup('me', tr.data$F1_Exec_Comp_Cog_Accuracy, tr.data)


# Now first report the number of times each thing variable is selected
male.vol.data <- vol.data[which(vol.data$sex==1),]
vol.col <- grep('mprage_jlf_vol', names(vol.data))
male.vol.values <- scale(male.vol.data[,vol.col])[,1:length(vol.col)]
male.vol.outcome <- scale(male.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.vol.outcome <- male.vol.outcome[complete.cases(male.vol.data[,vol.col])]
male.vol.values <- male.vol.values[complete.cases(male.vol.data[,vol.col]),]
male.vol.selection <- runMyLars(male.vol.values, male.vol.outcome, nCor=30, alphaSequence=1)
male.vol.sum <- as.matrix(returnSelectionCol(male.vol.selection))
male.vol.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.vol.values, male.vol.outcome)$coefficients)))
rownames(male.vol.enet.beta) <- colnames(male.vol.values)
rownames(male.vol.sum) <- colnames(male.vol.values)
male.vol.out <- cbind(male.vol.enet.beta, male.vol.sum)

# Now do female data 
female.vol.data <- vol.data[which(vol.data$sex==2),]
female.vol.values <- scale(female.vol.data[,vol.col])[,1:length(vol.col)]
female.vol.outcome <- scale(female.vol.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.vol.outcome <- female.vol.outcome[complete.cases(female.vol.data[,vol.col])]
female.vol.values <- female.vol.values[complete.cases(female.vol.data[,vol.col]),]
female.vol.selection <- runMyLars(female.vol.values, female.vol.outcome, nCor=30, alphaSequence=1)
female.vol.sum <- as.matrix(returnSelectionCol(female.vol.selection))
female.vol.lambda <- tuneAlpha(x=female.vol.values, y=female.vol.outcome, alphaSequence=1, nFolds=10)[2]
female.vol.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.vol.values, female.vol.outcome)$coefficients)))
rownames(female.vol.enet.beta) <- colnames(female.vol.values)
rownames(female.vol.sum) <- colnames(female.vol.values)
female.vol.out <- cbind(female.vol.enet.beta, female.vol.sum)

# Now do CBF
male.cbf.data <- cbf.data[which(cbf.data$sex==1),]
cbf.col <- grep('pcasl_jlf_cbf', names(cbf.data))
male.cbf.values <- scale(male.cbf.data[,cbf.col])[,1:length(cbf.col)]
male.cbf.outcome <- scale(male.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.cbf.outcome <- male.cbf.outcome[complete.cases(male.cbf.data[,cbf.col])]
male.cbf.values <- male.cbf.values[complete.cases(male.cbf.data[,cbf.col]),]
male.cbf.selection <- runMyLars(male.cbf.values, male.cbf.outcome, nCor=30, alphaSequence=1)
male.cbf.sum <- as.matrix(returnSelectionCol(male.cbf.selection))
male.cbf.lambda <- tuneAlpha(x=male.cbf.values, y=male.cbf.outcome, alphaSequence=1, nFolds=10)[2]
male.cbf.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.cbf.values, male.cbf.outcome)$coefficients)))
rownames(male.cbf.enet.beta) <- colnames(male.cbf.values)
rownames(male.cbf.sum) <- colnames(male.cbf.values)
male.cbf.out <- cbind(male.cbf.enet.beta, male.cbf.sum)

# Now do female data 
female.cbf.data <- cbf.data[which(cbf.data$sex==2),]
female.cbf.values <- scale(female.cbf.data[,cbf.col])[,1:length(cbf.col)]
female.cbf.outcome <- scale(female.cbf.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.cbf.outcome <- female.cbf.outcome[complete.cases(female.cbf.data[,cbf.col])]
female.cbf.values <- female.cbf.values[complete.cases(female.cbf.data[,cbf.col]),]
female.cbf.selection <- runMyLars(female.cbf.values, female.cbf.outcome, nCor=30, alphaSequence=1)
female.cbf.sum <- as.matrix(returnSelectionCol(female.cbf.selection))
female.cbf.lambda <- tuneAlpha(x=female.cbf.values, y=female.cbf.outcome, alphaSequence=1, nFolds=10)[2]
female.cbf.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.cbf.values, female.cbf.outcome)$coefficients)))
rownames(female.cbf.enet.beta) <- colnames(female.cbf.values)
rownames(female.cbf.sum) <- colnames(female.cbf.values)
female.cbf.out <- cbind(female.cbf.enet.beta, female.cbf.sum)

# Now onto GMD
male.gmd.data <- gmd.data[which(gmd.data$sex==1),]
gmd.col <- grep('mprage_jlf_gmd', names(gmd.data))
male.gmd.values <- scale(male.gmd.data[,gmd.col])[,1:length(gmd.col)]
male.gmd.outcome <- scale(male.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.gmd.outcome <- male.gmd.outcome[complete.cases(male.gmd.data[,gmd.col])]
male.gmd.values <- male.gmd.values[complete.cases(male.gmd.data[,gmd.col]),]
male.gmd.selection <- runMyLars(male.gmd.values, male.gmd.outcome, nCor=30, alphaSequence=1)
male.gmd.sum <- as.matrix(returnSelectionCol(male.gmd.selection))
male.gmd.lambda <- tuneAlpha(x=male.gmd.values, y=male.gmd.outcome, alphaSequence=1, nFolds=10)[2]
male.gmd.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.gmd.values, male.gmd.outcome)$coefficients)))
rownames(male.gmd.enet.beta) <- colnames(male.gmd.values)
rownames(male.gmd.sum) <- colnames(male.gmd.values)
male.gmd.out <- cbind(male.gmd.enet.beta, male.gmd.sum)

# Now do female data 
female.gmd.data <- gmd.data[which(gmd.data$sex==2),]
female.gmd.values <- scale(female.gmd.data[,gmd.col])[,1:length(gmd.col)]
female.gmd.outcome <- scale(female.gmd.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.gmd.outcome <- female.gmd.outcome[complete.cases(female.gmd.data[,gmd.col])]
female.gmd.values <- female.gmd.values[complete.cases(female.gmd.data[,gmd.col]),]
female.gmd.selection <- runMyLars(female.gmd.values, female.gmd.outcome, nCor=30, alphaSequence=1)
female.gmd.sum <- as.matrix(returnSelectionCol(female.gmd.selection))
female.gmd.lambda <- tuneAlpha(x=female.gmd.values, y=female.gmd.outcome, alphaSequence=1, nFolds=10)[2]
female.gmd.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.gmd.values, female.gmd.outcome)$coefficients)))
rownames(female.gmd.enet.beta) <- colnames(female.gmd.values)
rownames(female.gmd.sum) <- colnames(female.gmd.values)
female.gmd.out <- cbind(female.gmd.enet.beta, female.gmd.sum)

# Now onto trace data
male.tr.data <- tr.data[which(tr.data$sex==1),]
tr.col <- grep('dti_jlf_tr', names(tr.data))
male.tr.values <- scale(male.tr.data[,tr.col])[,1:length(tr.col)]
male.tr.outcome <- scale(male.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
male.tr.outcome <- male.tr.outcome[complete.cases(male.tr.data[,tr.col])]
male.tr.values <- male.tr.values[complete.cases(male.tr.data[,tr.col]),]
male.tr.selection <- runMyLars(male.tr.values, male.tr.outcome, nCor=30, alphaSequence=1)
male.tr.sum <- as.matrix(returnSelectionCol(male.tr.selection))
male.tr.lambda <- tuneAlpha(x=male.tr.values, y=male.tr.outcome, alphaSequence=1, nFolds=10)[2]
male.tr.enet.beta <- as.matrix(abs(as.numeric(mylars100(male.tr.values, male.tr.outcome)$coefficients)))
rownames(male.tr.enet.beta) <- colnames(male.tr.values)
rownames(male.tr.sum) <- colnames(male.tr.values)
male.tr.out <- cbind(male.tr.enet.beta, male.tr.sum)

# Now do female data 
female.tr.data <- tr.data[which(tr.data$sex==2),]
female.tr.values <- scale(female.tr.data[,tr.col])[,1:length(tr.col)]
female.tr.outcome <- scale(female.tr.data$F1_Exec_Comp_Cog_Accuracy)[,1]
female.tr.outcome <- female.tr.outcome[complete.cases(female.tr.data[,tr.col])]
female.tr.values <- female.tr.values[complete.cases(female.tr.data[,tr.col]),]
female.tr.selection <- runMyLars(female.tr.values, female.tr.outcome, nCor=30, alphaSequence=1)
female.tr.sum <- as.matrix(returnSelectionCol(female.tr.selection))
female.tr.lambda <- tuneAlpha(x=female.tr.values, y=female.tr.outcome, alphaSequence=1, nFolds=10)[2]
female.tr.enet.beta <- as.matrix(abs(as.numeric(mylars100(female.tr.values, female.tr.outcome)$coefficients)))
rownames(female.tr.enet.beta) <- colnames(female.tr.values)
rownames(female.tr.sum) <- colnames(female.tr.values)
female.tr.out <- cbind(female.tr.enet.beta, female.tr.sum)

# Now create the output
eNetBetas <- NULL
eNetBetas <- c(male.vol.out[,1], male.cbf.out[,1], male.gmd.out[,1], male.tr.out[,1])
tmp <- c(rep('Vol', length(male.vol.out[,1])), rep('CBF', length(male.cbf.out[,1])), rep('GMD', length(male.gmd.out[,1])), rep('TR', length(male.tr.out[,1])))
rowNames <- c(rownames(male.vol.out), rownames(male.cbf.out), rownames(male.gmd.out), rownames(male.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allmaleBetaMR.csv', quote=F)
maleBetaMR <- output

eNetBetas <- NULL
eNetBetas <- c(female.vol.out[,1], female.cbf.out[,1], female.gmd.out[,1], female.tr.out[,1])
tmp <- c(rep('Vol', length(female.vol.out[,1])), rep('CBF', length(female.cbf.out[,1])), rep('GMD', length(female.gmd.out[,1])), rep('TR', length(female.tr.out[,1])))
rowNames <- c(rownames(female.vol.out), rownames(female.cbf.out), rownames(female.gmd.out), rownames(female.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allfemaleBetaMR.csv', quote=F)
femaleBetaMR <- output

eNetBetas <- NULL
eNetBetas <- c(male.vol.out[,2], male.cbf.out[,2], male.gmd.out[,2], male.tr.out[,2])
tmp <- c(rep('Vol', length(male.vol.out[,1])), rep('CBF', length(male.cbf.out[,1])), rep('GMD', length(male.gmd.out[,1])), rep('TR', length(male.tr.out[,1])))
rowNames <- c(rownames(male.vol.out), rownames(male.cbf.out), rownames(male.gmd.out), rownames(male.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allmaleSelectNMR.csv', quote=F)
maleSelectMR <- output

eNetBetas <- NULL
eNetBetas <- c(female.vol.out[,2], female.cbf.out[,2], female.gmd.out[,2], female.tr.out[,2])
tmp <- c(rep('Vol', length(female.vol.out[,1])), rep('CBF', length(female.cbf.out[,1])), rep('GMD', length(female.gmd.out[,1])), rep('TR', length(female.tr.out[,1])))
rowNames <- c(rownames(female.vol.out), rownames(female.cbf.out), rownames(female.gmd.out), rownames(female.tr.out))
eNetBetas <- cbind(eNetBetas, tmp)#, rowNames)
for(i in c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'dti_jlf_tr_')){
  rownames(eNetBetas) <- gsub(x=rownames(eNetBetas), pattern=i, replacement='')
}
eNetBetas <- cbind(eNetBetas, rownames(eNetBetas))
rownames(eNetBetas) <- NULL
eNetBetas <- as.data.frame(eNetBetas)
output <- reshape(data=eNetBetas, timevar='tmp', idvar='V3', v.names='eNetBetas', direction='wide')
write.csv(output, 'allfemaleSelectNMR.csv', quote=F)
femaleSelectMR <- output

