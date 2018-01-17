# AFGR Jan 2018 - wow its a whole new year! 

# This script will be used to produce lm beta weights for the modality regressed values - with the intent of giving an effect
# size for each region predicting each cognitive outcome
# It will use the age regressed, meaned LR, and modality regressed values for:
#	vol, cbf, gmd, tr, and FA
# This will be performed w/in sex

## Load library(s)
install_load('psych')

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
