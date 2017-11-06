source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source("/home/adrose/hiLo/scripts/02_DevelopmentalTrends/functions/functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'cowplot')


# Load the data 
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
vol.data$ageAtGo1Scan <- vol.data$ageAtGo1Scan / 12
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
gmd.data$ageAtGo1Scan <- gmd.data$ageAtGo1Scan / 12
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ccData.csv')

# Now load the mgi data
vol.data.m <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/volumeData.csv")
gmd.data.m <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/gmdData.csv")
ct.data.m <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/ctData.csv")

# Now match the columns
vol.data <- vol.data[,(names(vol.data) %in% names(vol.data.m))]
vol.data.m <- vol.data.m[,(names(vol.data.m) %in% names(vol.data))]

# Now combine the data
vol.data <- rbind(vol.data, vol.data.m)
vol.data$ageBin <- 'Childhood'
vol.data$ageBin[vol.data$ageAtGo1Scan > 14.1] <- 'Adolescence'
vol.data$ageBin[vol.data$ageAtGo1Scan > 18.1] <- 'Early Adulthood'
vol.data$ageBin[vol.data$ageAtGo1Scan > 29.1] <- '29-55'
vol.data$ageBin[vol.data$ageAtGo1Scan > 56] <- '56+'

volume.data.1 <- rbind(returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Childhood"), returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Adolescence"), returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Early Adulthood"), returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","29-55"), returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","56+"))
volume.data.1$ageBin <- factor(volume.data.1$ageBin, levels=c('56+', '29-55','Early Adulthood','Adolescence','Childhood'))
volumePlot <- createGGPlotImage(volume.data.1, 'Volume', -2, 2, .2)

gmd.data <- gmd.data[,(names(gmd.data) %in% names(gmd.data.m))]
gmd.data.m <- gmd.data.m[,(names(gmd.data.m) %in% names(gmd.data))]

# Now combine the data
gmd.data <- rbind(gmd.data, gmd.data.m)
gmd.data$ageBin <- 'Childhood'
gmd.data$ageBin[gmd.data$ageAtGo1Scan > 14.1] <- 'Adolescence'
gmd.data$ageBin[gmd.data$ageAtGo1Scan > 18.1] <- 'Early Adulthood'
gmd.data$ageBin[gmd.data$ageAtGo1Scan > 29.1] <- '29-55'
gmd.data$ageBin[gmd.data$ageAtGo1Scan > 56] <- '56+'

gmd.data.1 <- rbind(returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Childhood"), returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Adolescence"), returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Early Adulthood"), returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","29-55"), returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","56+"))
gmd.data.1$ageBin <- factor(gmd.data.1$ageBin, levels=c('56+', '29-55','Early Adulthood','Adolescence','Childhood'))
gmdPlot <- createGGPlotImage(gmd.data.1, 'gmd', -2, 2.2, .2)


ct.data <- ct.data[,(names(ct.data) %in% names(ct.data.m))]
ct.data.m <- ct.data.m[,(names(ct.data.m) %in% names(ct.data))]

# Now combine the data
ct.data <- rbind(ct.data, ct.data.m)
ct.data$ageBin <- 'Childhood'
ct.data$ageBin[ct.data$ageAtGo1Scan > 14.1] <- 'Adolescence'
ct.data$ageBin[ct.data$ageAtGo1Scan > 18.1] <- 'Early Adulthood'
ct.data$ageBin[ct.data$ageAtGo1Scan > 29.1] <- '29-55'
ct.data$ageBin[ct.data$ageAtGo1Scan > 56] <- '56+'

ct.data.1 <- rbind(returnStandardizedGenderMeansCT(ct.data, "mprage_jlf_ct", "","Childhood"), returnStandardizedGenderMeansCT(ct.data, "mprage_jlf_ct", "","Adolescence"), returnStandardizedGenderMeansCT(ct.data, "mprage_jlf_ct", "","Early Adulthood"), returnStandardizedGenderMeansCT(ct.data, "mprage_jlf_ct", "","29-55"), returnStandardizedGenderMeansCT(ct.data, "mprage_jlf_ct", "","56+"))
ct.data.1$ageBin <- factor(ct.data.1$ageBin, levels=c('56+', '29-55','Early Adulthood','Adolescence','Childhood'))
ctPlot <- createGGPlotImage(ct.data.1, 'ct', -2.2, 2, .2)

pdf('developmentalFigures.pdf' ,height=34, width=20)
volumePlot
gmdPlot
ctPlot
dev.off()
