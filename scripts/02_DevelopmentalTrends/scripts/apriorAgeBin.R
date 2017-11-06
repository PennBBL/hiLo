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

pdf('developmentalFigures.pdf' ,height=34, width=20)
volumePlot
dev.off()
