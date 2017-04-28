# AFGR October 29th 2015

# This script will be used to produce graphs which will plot the z scored to the global population for each # sex
# It will then plot them in the hi lo method where roi's are the x axis and the difference is the y axis

## Load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source("/home/adrose/produceMeanGenderGraphsJLF/scripts/functions.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'cowplot')


## Load data here
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

## Now attach age bin to all of our data 
vol.data <- addAgeBin(vol.data, vol.data$ageAtGo1Scan, 167, 215, 216)
cbf.data <- addAgeBin(cbf.data, cbf.data$ageAtGo1Scan, 167, 215, 216)
gmd.data <- addAgeBin(gmd.data, gmd.data$ageAtGo1Scan, 167, 215, 216)
ct.data <- addAgeBin(ct.data, ct.data$ageAtGo1Scan, 167, 215, 216)
reho.data <- addAgeBin(reho.data, reho.data$ageAtGo1Scan, 167, 215, 216)
alff.data <- addAgeBin(alff.data, alff.data$ageAtGo1Scan, 167, 215, 216)
ad.data <- addAgeBin(ad.data, ad.data$ageAtGo1Scan, 167, 215, 216)
fa.data <- addAgeBin(fa.data, fa.data$ageAtGo1Scan, 167, 215, 216)
rd.data <- addAgeBin(rd.data, rd.data$ageAtGo1Scan, 167, 215, 216)
tr.data <- addAgeBin(tr.data, tr.data$ageAtGo1Scan, 167, 215, 216)

# Cretae the volume data
volume.data.1 <- returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Childhood")
volume.data.1 <- rbind(volume.data.1, returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Adolescence"))
volume.data.1 <- rbind(volume.data.1, returnStandardizedGenderMeans(vol.data, "mprage_jlf_vol", "","Early Adulthood"))
volume.data.1$ageBin <- factor(volume.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now do CBF
cbf.data.1 <- returnStandardizedGenderMeans(cbf.data, "pcasl_jlf_cbf", "","Childhood")
cbf.data.1 <- rbind(cbf.data.1, returnStandardizedGenderMeans(cbf.data, "pcasl_jlf_cbf", "","Adolescence"))
cbf.data.1 <- rbind(cbf.data.1, returnStandardizedGenderMeans(cbf.data, "pcasl_jlf_cbf", "","Early Adulthood"))
cbf.data.1$ageBin <- factor(cbf.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now do GMD
gmd.data.1 <- returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Childhood")
gmd.data.1 <- rbind(gmd.data.1, returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Adolescence"))
gmd.data.1 <- rbind(gmd.data.1, returnStandardizedGenderMeans(gmd.data, "mprage_jlf_gmd", "","Early Adulthood"))
gmd.data.1$ageBin <- factor(gmd.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now do ReHo
reho.data.1 <- returnStandardizedGenderMeans(reho.data, "rest_jlf_reho", "","Childhood")
reho.data.1 <- rbind(reho.data.1, returnStandardizedGenderMeans(reho.data, "rest_jlf_reho", "","Adolescence"))
reho.data.1 <- rbind(reho.data.1, returnStandardizedGenderMeans(reho.data, "rest_jlf_reho", "","Early Adulthood"))
reho.data.1$ageBin <- factor(reho.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now do ALFF
alff.data.1 <- returnStandardizedGenderMeans(alff.data, "rest_jlf_alff", "","Childhood")
alff.data.1 <- rbind(alff.data.1, returnStandardizedGenderMeans(alff.data, "rest_jlf_alff", "","Adolescence"))
alff.data.1 <- rbind(alff.data.1, returnStandardizedGenderMeans(alff.data, "rest_jlf_alff", "","Early Adulthood"))
alff.data.1$ageBin <- factor(alff.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# And finish strong with CT
ct.data.1 <- returnStandardizedGenderMeansCT(ct.data, 'mprage_jlf_ct', '', 'Childhood')
ct.data.1 <- rbind(ct.data.1, returnStandardizedGenderMeansCT(ct.data, 'mprage_jlf_ct', '', 'Adolescence'))
ct.data.1 <- rbind(ct.data.1, returnStandardizedGenderMeansCT(ct.data, 'mprage_jlf_ct', '', 'Early Adulthood'))
ct.data.1$ageBin <- factor(ct.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Wait there is more... now do all of the DTI GM regions
# Start with AD
ad.data.1 <- returnStandardizedGenderMeans(ad.data, 'dti_jlf_ad', '', 'Childhood')
ad.data.1 <- rbind(ad.data.1, returnStandardizedGenderMeans(ad.data, 'dti_jlf_ad', '', 'Adolescence'))
ad.data.1 <- rbind(ad.data.1, returnStandardizedGenderMeans(ad.data, 'dti_jlf_ad', '', 'Early Adulthood'))
ad.data.1$ageBin <- factor(ad.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now do FA
fa.data.1 <- returnStandardizedGenderMeans(fa.data, 'dti_jlf_fa', '', 'Childhood')
fa.data.1 <- rbind(fa.data.1, returnStandardizedGenderMeans(fa.data, 'dti_jlf_fa', '', 'Adolescence'))
fa.data.1 <- rbind(fa.data.1, returnStandardizedGenderMeans(fa.data, 'dti_jlf_fa', '', 'Early Adulthood'))
fa.data.1$ageBin <- factor(fa.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now onto RD
rd.data.1 <- returnStandardizedGenderMeans(rd.data, 'dti_jlf_rd', '', 'Childhood')
rd.data.1 <- rbind(rd.data.1, returnStandardizedGenderMeans(rd.data, 'dti_jlf_rd', '', 'Adolescence'))
rd.data.1 <- rbind(rd.data.1, returnStandardizedGenderMeans(rd.data, 'dti_jlf_rd', '', 'Early Adulthood'))
rd.data.1$ageBin <- factor(rd.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))

# Now onto TR
tr.data.1 <- returnStandardizedGenderMeans(tr.data, 'dti_jlf_tr', '', 'Childhood')
tr.data.1 <- rbind(tr.data.1, returnStandardizedGenderMeans(tr.data, 'dti_jlf_tr', '', 'Adolescence'))
tr.data.1 <- rbind(tr.data.1, returnStandardizedGenderMeans(tr.data, 'dti_jlf_tr', '', 'Early Adulthood'))
tr.data.1$ageBin <- factor(tr.data.1$ageBin, levels=c('Early Adulthood','Adolescence','Childhood'))


volumePlot <- createGGPlotImage(volume.data.1, 'Volume', -.8, .9, .2)
cbfPlot <- createGGPlotImage(cbf.data.1, 'CBF', -.9, .8, .2)
gmdPlot <- createGGPlotImage(gmd.data.1, 'GMD', -.8, .9, .2)
ctPlot <- createGGPlotImage(ct.data.1, 'CT', -.9, .8, .2)
rehoPlot <- createGGPlotImage(reho.data.1, 'ReHo', -.8, .8, .2)
alffPlot <- createGGPlotImage(alff.data.1, 'ALFF', -.7, .9, .2)
adPlot <- createGGPlotImage(ad.data.1, 'Axial Diffusitivity', -1.2, .8, .2)
#faPlot <- createGGPlotImage(fa.data.1, 'Fractional Anisotropy', -.8, .8, .2)
rdPlot <- createGGPlotImage(rd.data.1, 'Radial Diffusitivity', -1.2, .8, .2)
trPlot <- createGGPlotImage(tr.data.1, 'Trace (Mean Diffusitivity)', -1.2, .8, .2)

# Make a blank image
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

# Now plot them all
pdf('developmentalFigures.pdf' ,height=20, width=20)
volumePlot
cbfPlot
gmdPlot
ctPlot
rehoPlot
alffPlot
adPlot
#faPlot
rdPlot
trPlot
dev.off()


## Now do the WM labels down here 
## Load the data
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuADLabelsData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuFALabelsData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuRDLabelsData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuTRLabelsData.csv')

## Now attach age bin to all of our data 
ad.data <- addAgeBin(ad.data, ad.data$ageAtGo1Scan, 167, 215, 216)
fa.data <- addAgeBin(fa.data, fa.data$ageAtGo1Scan, 167, 215, 216)
rd.data <- addAgeBin(rd.data, rd.data$ageAtGo1Scan, 167, 215, 216)
tr.data <- addAgeBin(tr.data, tr.data$ageAtGo1Scan, 167, 215, 216)

# Now process all of the data
# STart with AD
colnames(ad.data)[69] <- 'dti_dtitk_jhulabel_ad_rlic'
ad.data.1 <- returnStandardizedWM1GenderMeans(ad.data, "Childhood")
ad.data.1 <- rbind(ad.data.1, returnStandardizedWM1GenderMeans(ad.data, "Adolescence"))
ad.data.1 <- rbind(ad.data.1, returnStandardizedWM1GenderMeans(ad.data, "Early Adulthood"))
ad.data.1$ageBin <- factor(ad.data.1$ageBin, levels=c("Childhood","Adolescence", "Early Adulthood"))
ad.data.1<- ad.data.1[-which(ad.data.1$ROI=='tap'),]

# Now do FA
colnames(fa.data)[69] <- 'dti_dtitk_jhulabel_fa_rlic'
fa.data.1 <- returnStandardizedWM1GenderMeans(fa.data, "Childhood")
fa.data.1 <- rbind(fa.data.1, returnStandardizedWM1GenderMeans(fa.data, "Adolescence"))
fa.data.1 <- rbind(fa.data.1, returnStandardizedWM1GenderMeans(fa.data, "Early Adulthood"))
fa.data.1$ageBin <- factor(fa.data.1$ageBin, levels=c("Childhood","Adolescence", "Early Adulthood"))
fa.data.1<- fa.data.1[-which(fa.data.1$ROI=='tap'),]

# Now onto RD 
# RD is curretnly broken atm because of my average across hemishpere function
rd.data.1 <- returnStandardizedWM1GenderMeans(rd.data, "Childhood")
rd.data.1 <- rbind(rd.data.1, returnStandardizedWM1GenderMeans(rd.data, "Adolescence"))
rd.data.1 <- rbind(rd.data.1, returnStandardizedWM1GenderMeans(rd.data, "Early Adulthood"))
rd.data.1$ageBin <- factor(rd.data.1$ageBin, levels=c("Childhood","Adolescence", "Early Adulthood"))
rd.data.1<- rd.data.1[-which(rd.data.1$ROI=='tap'),]

# Now do TR
colnames(tr.data)[69] <- "dti_dtitk_jhulabel_tr_rlic"
tr.data.1 <- returnStandardizedWM1GenderMeans(tr.data, "Childhood")
tr.data.1 <- rbind(tr.data.1, returnStandardizedWM1GenderMeans(tr.data, "Adolescence"))
tr.data.1 <- rbind(tr.data.1, returnStandardizedWM1GenderMeans(tr.data, "Early Adulthood"))
tr.data.1$ageBin <- factor(tr.data.1$ageBin, levels=c("Childhood","Adolescence", "Early Adulthood"))
tr.data.1<- tr.data.1[-which(tr.data.1$ROI=='tap'),]

# Now plot these suckers 
adPlot <- createGGPlotImage(ad.data.1, 'WM Labels Axial Diffusitivity', -1, 1, .2)
faPlot <- createGGPlotImage(fa.data.1, 'WM Labels Fractional Anisotropy', -1, 1, .2)
rdPlot <- createGGPlotImage(rd.data.1, 'Radial Diffusitivity', -1.2, 1, .2)
trPlot <- createGGPlotImage(tr.data.1, 'WM Labels Trace (Mean Diffusitivity)', -1, 1, .2)

# Now plot them all
pdf('developmentalFiguresWMLabels.pdf' ,height=20, width=20)
adPlot
faPlot
rdPlot
trPlot
dev.off()
