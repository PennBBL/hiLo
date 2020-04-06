### This script creates a correlation plot of the global values
###
### Ellyn Butler
### February 27, 2020 - April 6, 2020

library('ggplot2')

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
  tmp <- tmp + vol.data[,volregs[i]]*img.data[,idemoregs[i]]
}
tmp <- tmp/sum(vol.data[, volregs])
img.data$sigchange_cope1_task_mean_miccai_ave_AAE <- tmp


img.data <- img.data[, c(global.values, "sex")]
colnames(img.data) <- c("TBV", "GMD", "MD", "CBF", "ALFF", "ReHo", "NB MFG", "Id AAE", "sex")

#### Get Bonferroni-corrected p
corr <- data.frame(matrix(NA, nrow=8, ncol=8))
names(corr) <- c("TBV", "GMD", "MD", "CBF", "ALFF", "ReHo", "NB MFG", "Id AAE")
row.names(corr) <- names(corr)

sigfunc <- function(pv, corr, corr_val) {
  corr_val <- round(corr_val, digits=3)
  if (pv < .001) {
    sig <- "***"
    corr[i, j] <- paste0(corr_val, sig)
  } else if (pv < .01) {
    sig <- "**"
    corr[i, j] <- paste0(corr_val, sig)
  } else if (pv < .05) {
    sig <- "*"
    corr[i, j] <- paste0(corr_val, sig)
  } else {
    corr[i, j] <- corr_val
  }
  corr
}

# Number of comparisons: 56
for (i in 1:ncol(corr)) {
  for (j in 1:ncol(corr)) {
    if (j < i) { # Female
      female_test <- cor.test(img.data[img.data$sex == 2, names(img.data)[i]],
        img.data[img.data$sex == 2, names(img.data)[j]])
      pv <- (female_test$p.value)*56
      corr_val <- female_test$estimate[[1]]
      corr <- sigfunc(pv, corr, corr_val)
    } else if (j > i) { # Male
      male_test <- cor.test(img.data[img.data$sex == 1, names(img.data)[i]],
        img.data[img.data$sex == 1, names(img.data)[j]])
      pv <- (male_test$p.value)*56
      corr_val <- male_test$estimate[[1]]
      corr <- sigfunc(pv, corr, corr_val)
    }
  }
}


write.csv(corr, "~/Documents/hiLo/tables/globalCorrSex.csv")
