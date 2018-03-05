## Load library(s)
install_load('psych', 'ggplot2', 'ggrepel', 'reshape')
source("/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R")

## Load data
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
vol.data <- vol.data[,-grep('mprage_jlf_vol_ICV', names(vol.data))]
vol.data <- vol.data[,-c(48:51)]
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/gmdData.csv')
gmd.data <- gmd.data[,-grep('gmd_MeanGMD', names(gmd.data))]
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfTRData.csv')
tr.data <- tr.data[,-grep('tr_MeanTR', names(tr.data))]
fa.data.wm <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jhuFALabel.csv')

## Now for each modality go through a loop of 100 times and compare the importance metrics with those from the original analysis
## which includes modal reg values including every ROI
data.vals <- c('vol.data', 'cbf.data', 'gmd.data', 'reho.data', 'alff.data', 'tr.data')
cut.vals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'rest_jlf_reho_', 'rest_jlf_alff_', 'dti_jlf_tr_')
fact.vals <- names(vol.data)[4:16]
output <- matrix(NA, 134, 100)
rownames(output) <- c(gsub(x=names(vol.data)[grep('_jlf_', colnames(vol.data))], pattern='mprage_jlf_vol_', replacement=''))
for(v in 1:100){
  for(s in c(1)){
    for(fact in fact.vals[1]){
      for(modal in 1){
          # Grabb our data and isolate IV and DV's
          tmpDF <- get(data.vals[modal])
          tmpDF <- tmpDF[which(tmpDF$sex==s),]
          tmpY <- tmpDF[,fact]
          tmpX <- as.matrix(scale(tmpDF[,grep('_jlf_', names(tmpDF))]))
          # Now remove a random 12 ROI's
          rand.rm <- sample(x=seq(1,dim(tmpX)[2]), 12, replace=F)
          # Now produce our beta weights annd then give em ranks
          tmpX <- regressWithinModality(tmpX[,-rand.rm], '_jlf_')
          tmpIn <- as.data.frame(cbind(tmpY, tmpX))
          mod <- lm(tmpY ~ ., data=tmpIn)
          outputBeta <- rank(coefficients(mod)[-1])
          # Now write these ranks to our output file
          names(outputBeta) <- gsub(names(outputBeta), pattern=cut.vals[modal], replacement='')
          output[match(names(outputBeta), rownames(output)),v] <- outputBeta[complete.cases(match(names(outputBeta), rownames(output)))]        
      }
    }
  }
}

## Now I need to make a violin plot for all of these values
plotVals <- melt(output)
plotVals <- as.data.frame(plotVals)
orderVal <- summarySE(data=plotVals, groupvars='X1', measurevar='value', na.rm=T)
plotVals$X1 = factor(plotVals$X1, levels = as.character(orderVal$X1[order(orderVal$value)]))
outPlot <- ggplot(plotVals, aes(x=X1, y=value)) + 
  geom_violin() +
  theme(axis.text.x=element_text(angle=90), 
        text=(element_text(size=20))) + 
  labs(y="Importannce Rank", x='ROI')
pdf("test.pdf", height=30, width=60)
print(outPlot)
dev.off()
