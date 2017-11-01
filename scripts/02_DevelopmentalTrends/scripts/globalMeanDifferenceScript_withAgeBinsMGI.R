## Load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source("/home/adrose/hiLo/scripts/02_DevelopmentalTrends/functions/functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'cowplot')

# Now load the data 
vol.data <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/volumeData.csv")
gmd.data <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/gmdData.csv")
ct.data <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/ctData.csv")

# Now add age bins
ageBinVals <- floor(quantile(vol.data$ageAtGo1Scan, c(.33, .66, 1)))
vol.data <- addAgeBin(vol.data, vol.data$ageAtGo1Scan, ageBinVals[1], ageBinVals[2], ageBinVals[2]+.01)
gmd.data <- addAgeBin(gmd.data, gmd.data$ageAtGo1Scan, ageBinVals[1], ageBinVals[2], ageBinVals[2]+.01)
ct.data  <- addAgeBin(ct.data, ct.data$ageAtGo1Scan, ageBinVals[1], ageBinVals[2], ageBinVals[2]+.01)

# Now produce the values
volume.data.1 <- returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Childhood")
volume.data.1 <- rbind(volume.data.1, returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Adolescence"))
volume.data.1 <- rbind(volume.data.1, returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Early Adulthood"))
volume.data.1$ageBin <- revalue(volume.data.1$ageBin, replace=c('Early Adulthood'="50-85",'Adolescence'='29.1-49.9','Childhood'='12-29'))
volume.data.1$ageBin <- factor(volume.data.1$ageBin, levels=c('50-85','29.1-49.9','12-29'))

# Now do GMD
gmd.data.1 <- returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Childhood")
gmd.data.1 <- rbind(gmd.data.1, returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Adolescence"))
gmd.data.1 <- rbind(gmd.data.1, returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Early Adulthood"))
gmd.data.1$ageBin <- revalue(gmd.data.1$ageBin, replace=c('Early Adulthood'="50-85",'Adolescence'='29.1-49.9','Childhood'='12-29'))
gmd.data.1$ageBin <- factor(gmd.data.1$ageBin, levels=c('50-85','29.1-49.9','12-29'))

# Now do CT
ct.data.1 <- returnStandardizedGenderMeansCT(ct.data, 'mprage_jlf_ct', '', 'Childhood')
ct.data.1 <- rbind(ct.data.1, returnStandardizedGenderMeansCT(ct.data, 'mprage_jlf_ct', '', 'Adolescence'))
ct.data.1 <- rbind(ct.data.1, returnStandardizedGenderMeansCT(ct.data, 'mprage_jlf_ct', '', 'Early Adulthood'))
ct.data.1$ageBin <- revalue(ct.data.1$ageBin, replace=c('Early Adulthood'="50-85",'Adolescence'='29.1-49.9','Childhood'='12-29'))
ct.data.1$ageBin <- factor(ct.data.1$ageBin, levels=c('50-85','29.1-49.9','12-29'))

volumePlot <- createGGPlotImage(volume.data.1, 'Volume', -1, 1.4, .2)
gmdPlot <- createGGPlotImage(gmd.data.1, 'GMD', -1, 1.4, .2)
ctPlot <- createGGPlotImage(ct.data.1, 'CT', -1, 1.4, .2)

pdf('mgiAgeTrends.pdf')
volumePlot
gmdPlot
ctPlot
dev.off()

