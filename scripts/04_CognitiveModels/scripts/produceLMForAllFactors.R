# AFGR Jan 2018 - wow its a whole new year! 

# This script will be used to produce lm beta weights for the modality regressed values - with the intent of giving an effect
# size for each region predicting each cognitive outcome
# It will use the age regressed, meaned LR, and modality regressed values for:
#	vol, cbf, gmd, tr, and FA
# This will be performed w/in sex

## Load library(s)
install_load('psych', 'ggplot2', 'ggrepel')

## Load data
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalRegQA/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalRegQA/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalReg/jlfFAData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegModalRegQA/jlfTRData.csv')

# Now create a series of loops which will go through each DF, and each cog outcome and return beta weights
data.vals <- c('vol.data', 'cbf.data', 'gmd.data', 'reho.data', 'alff.data', 'tr.data')
cut.vals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_tr_')
fact.vals <- names(vol.data)[4:16]
# Now write the loop
for(w in fact.vals){
  fac.vals <- w
  output <- matrix(NA, 138, 12)
  rownames(output) <- c(gsub(x=names(vol.data)[grep('_jlf_', colnames(vol.data))], pattern='mprage_jlf_vol_', replacement=''))
  colnames(output) <- c(paste(data.vals, 'Male'), paste(data.vals, 'Female'))
  for(g in 1){
    for(dat in 1:length(data.vals)){
      for(fac in fac.vals){
        datIn <- data.vals[dat] 
        tmpDF <- get(datIn)
        tmpDF <- tmpDF[which(tmpDF$sex==g),]
        print(fac)
        tmpY <- tmpDF[,fac]
        tmpX <- as.matrix(scale(tmpDF[,grep('_jlf_', names(tmpDF))]))
        tmpIn <- as.data.frame(cbind(tmpY, tmpX))
        mod <- lm(tmpY ~ ., data=tmpIn)
        outputBeta <- coefficients(mod)[-1]
        names(outputBeta) <- gsub(names(outputBeta), pattern=cut.vals[dat], replacement='')
        output[match(names(outputBeta), rownames(output)),dat] <- outputBeta[complete.cases(match(names(outputBeta), rownames(output)))]
      }     
    }
  }
  for(g in 2){
    for(dat in 1:length(data.vals)){
      for(fac in fac.vals){
        datIn <- data.vals[dat] 
        tmpDF <- get(datIn)
        tmpDF <- tmpDF[which(tmpDF$sex==g),]
        tmpY <- tmpDF[,fac]
        tmpX <- as.matrix(scale(tmpDF[,grep('_jlf_', names(tmpDF))]))
        tmpIn <- as.data.frame(cbind(tmpY, tmpX))
        mod <- lm(tmpY ~ ., data=tmpIn)
        outputBeta <- coefficients(mod)[-1]
        names(outputBeta) <- gsub(names(outputBeta), pattern=cut.vals[dat], replacement='')
        output[match(names(outputBeta), rownames(output)),dat+6] <- outputBeta[complete.cases(match(names(outputBeta), rownames(output)))]
      }     
    }
  }
  write.csv(output, paste(fac.vals, 'BetaWeights.csv', sep=''), quote=F)
}

# Now loop through each output csv and combine the values 
orig <- read.csv(paste(fact.vals[1], 'BetaWeights.csv', sep=''))
colnames(orig)[2:13] <- paste(colnames(orig)[2:13], '.', fact.vals[1], sep='')
for(w in fact.vals[2:13]){
  print(w)
  # First thing we have to do is read in the csv
  csv.input <- read.csv(paste(w, 'BetaWeights.csv', sep=''))
  colnames(csv.input)[2:13] <- paste(colnames(csv.input)[2:13], '.', w, sep='')
  orig <- merge(orig, csv.input)
}

# Now make a rank output
origRank <- orig
origRank[,2:157] <- apply(-abs(origRank[,-1]), 2, function(x) rank(x, na.last='keep'))

# Now write the outputs
write.csv(orig, "allFactorBetaValues.csv", quote=F, row.names=F)
write.csv(origRank, "allFactorValuesBetaRank.csv", quote=F, row.names=F)

# Now prepare some scatter plots for the ranks
# First loop through all of the male 
stepOne <- colnames(origRank)#[grep("*curacy", colnames(origRank))]
stepTwo <- stepOne[grep("vol",stepOne)]
stepThree <- stepTwo[grep("Male", stepTwo)]
pdf('maleAccuracyRanks.pdf', height=20, width=20)
for(q in stepThree){
  toPlotMale <- ggplot(origRank,aes(x=vol.data.Male.F1_Exec_Comp_Cog_Accuracy,y=origRank[,grep(q, colnames(origRank))],label=X)) + 
    geom_point() + 
    geom_text_repel() +
    ylab(q) + 
    geom_smooth(method=lm) +
    geom_vline(xintercept = 0 , linetype=3)
  print(toPlotMale)
}
dev.off()
pdf('maleAccuracyBetas.pdf', height=20, width=20)
for(q in stepThree){
  toPlotMale <- ggplot(orig,aes(x=vol.data.Male.F1_Exec_Comp_Cog_Accuracy,y=orig[,grep(q, colnames(origRank))],label=X)) + 
    geom_point() + 
    geom_text_repel() +
    ylab(q) + 
    geom_smooth(method=lm) +
    geom_vline(xintercept = 0 , linetype=3)
  print(toPlotMale)
}
dev.off()
