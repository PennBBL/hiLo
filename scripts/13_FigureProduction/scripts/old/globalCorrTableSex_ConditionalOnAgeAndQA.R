### This script creates a correlation plot of the global values
###
### Ellyn Butler
### June 26, 2020

library('ggplot2')
library('sjPlot')

# Load csv with Adon's global metrics
img.data <- read.csv('~/Documents/hiLo/data/n1601_hiLoDataDump_2018-09-20.csv')
img.data <- img.data[-which(is.na(img.data$F1_Exec_Comp_Cog_Accuracy)),]
img.data <- img.data[-which(abs(img.data$ageAtCnb1 - img.data$ageAtScan1 ) > 12),]
rownames(img.data) <- 1:nrow(img.data)

# Load nback and idemo data
nback.data <- read.csv('~/Documents/hiLo/data/meanLR/nbackData.csv')
idemo.data <- read.csv('~/Documents/hiLo/data/meanLR/idemoData.csv')

# Merge data
img.data <- merge(img.data, nback.data)
img.data <- merge(img.data, idemo.data)

# Filter data
global.values <- c('mprage_jlf_vol_TBV','mprage_jlf_gmd_MeanGMD','dti_jlf_tr_MeanTR',
  'pcasl_jlf_cbf_MeanGMCBF','rest_jlf_alff_MeanALFF','rest_jlf_reho_MeanReho',
  'sigchange_contrast4_2back0back_mean_miccai_ave_MFG',
  'sigchange_cope1_task_mean_miccai_ave_AAE')
img.data <- img.data[img.data$dti64Exclude == 0 & img.data$restExclude == 0 &
  img.data$t1Exclude == 0 & img.data$pcaslExclude == 0, ]
img.data <- img.data[!is.na(img.data$mprage_jlf_vol_TBV) & !is.na(img.data$mprage_jlf_gmd_MeanGMD) &
  !is.na(img.data$dti_jlf_tr_MeanTR) & !is.na(img.data$pcasl_jlf_cbf_MeanGMCBF) &
  !is.na(img.data$rest_jlf_alff_MeanALFF) & !is.na(img.data$rest_jlf_reho_MeanReho) &
  !is.na(img.data$sigchange_contrast4_2back0back_mean_miccai_ave_MFG) &
  !is.na(img.data$sigchange_cope1_task_mean_miccai_ave_FuG),]
row.names(img.data) <- 1:nrow(img.data)

# Get (subset of) limbic IdEmo activation
vol.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
shortregs <-  c("AIns", "Amygdala", "Ent")
volregs <- paste0("mprage_jlf_vol_", shortregs)
idemoregs <- paste0("sigchange_cope1_task_mean_miccai_ave_", shortregs)
vol.data <- vol.data[vol.data$bblid %in% img.data$bblid,]
row.names(img.data) <- 1:nrow(img.data)
row.names(vol.data) <- 1:nrow(vol.data)

tmp <- 0
for (i in 1:length(volregs)) {
  print(length(vol.data[,volregs[i]]))
  print(length(img.data[,idemoregs[i]]))
  tmp <- tmp + vol.data[,volregs[i]]*img.data[,idemoregs[i]]
}
tmp <- tmp/sum(vol.data[, volregs])
img.data$sigchange_cope1_task_mean_miccai_ave_AAE <- tmp


img.data <- img.data[, c("bblid", "sex", "ageAtGo1Scan","restRelMeanRMSMotion",
  global.values)]


# Compare relationship between TBV and MD before and after controlling for age
mod1 <- lm(scale(mprage_jlf_vol_TBV) ~ scale(ageAtGo1Scan) + scale(dti_jlf_tr_MeanTR),
  data=img.data)

mod2 <- lm(scale(mprage_jlf_vol_TBV) ~ scale(dti_jlf_tr_MeanTR),
    data=img.data)

tab_model(mod2, mod1)

# Compare relationship between ReHo and ALFF before and after controlling for motion
mod3 <- lm(scale(rest_jlf_alff_MeanALFF) ~ scale(restRelMeanRMSMotion) +
  scale(rest_jlf_reho_MeanReho), data=img.data)

mod4 <- lm(scale(rest_jlf_alff_MeanALFF) ~ scale(rest_jlf_reho_MeanReho),
    data=img.data)

tab_model(mod4, mod3)
