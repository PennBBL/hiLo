### This script creates a correlation plot of the global values
###
### Ellyn Butler
### February 27, 2020 - March 4, 2020

library('ggplot2')
library('ggcorrplot')

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
global.values <- c('mprage_jlf_vol_ICV','mprage_jlf_gmd_MeanGMD','dti_jlf_tr_MeanTR',
  'pcasl_jlf_cbf_MeanGMCBF','rest_jlf_alff_MeanALFF','rest_jlf_reho_MeanReho',
  'sigchange_contrast4_2back0back_mean_miccai_ave_MFG',
  'sigchange_cope1_task_mean_miccai_ave_Limbic')
img.data <- img.data[img.data$dti64Exclude == 0 & img.data$restExclude == 0 &
  img.data$t1Exclude == 0 & img.data$pcaslExclude == 0, ]
img.data <- img.data[!is.na(img.data$mprage_jlf_vol_ICV) & !is.na(img.data$mprage_jlf_gmd_MeanGMD) &
  !is.na(img.data$dti_jlf_tr_MeanTR) & !is.na(img.data$pcasl_jlf_cbf_MeanGMCBF) &
  !is.na(img.data$rest_jlf_alff_MeanALFF) & !is.na(img.data$rest_jlf_reho_MeanReho) &
  !is.na(img.data$sigchange_contrast4_2back0back_mean_miccai_ave_MFG) &
  !is.na(img.data$sigchange_cope1_task_mean_miccai_ave_FuG),]
rownames(img.data) <- 1:nrow(img.data)

# Get limbic IdEmo activation
vol.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
shortregs <-  c("ACgG", "PCgG", "AIns", "Amygdala", "Ent", "Hippocampus", "MCgG", "PHG", "PIns", "SCA")
volregs <- paste0("mprage_jlf_vol_", shortregs)
idemoregs <- paste0("sigchange_cope1_task_mean_miccai_ave_", shortregs)
vol.data <- vol.data[vol.data$bblid %in% img.data$bblid,]
rownames(img.data) <- 1:nrow(img.data)
rownames(vol.data) <- 1:nrow(vol.data)

tmp <- 0
for (i in 1:length(volregs)) {
  tmp <- tmp + vol.data[,volregs[i]]*img.data[,idemoregs[i]]
}
tmp <- tmp/sum(vol.data[, volregs])
img.data$sigchange_cope1_task_mean_miccai_ave_Limbic <- tmp

# Makes plots to show limbic is not good
for (i in 1:length(idemoregs)) {
  p_tmp <- ggplot(img.data, aes_string(idemoregs[i])) +
    geom_histogram(bins=100) + theme_linedraw() + ggtitle(paste0("IdEmo: ", shortregs[i])) +
    coord_cartesian(xlim=c(-2, 2))
  assign(paste0("p_", shortregs[i]), p_tmp)
}

p_limbic <- ggplot(img.data, aes(sigchange_cope1_task_mean_miccai_ave_Limbic)) +
  geom_histogram(bins=100) + theme_linedraw() + ggtitle("IdEmo: Limbic")
p_FuG <- ggplot(img.data, aes(sigchange_cope1_task_mean_miccai_ave_FuG)) +
  geom_histogram(bins=100) + theme_linedraw() + ggtitle("IdEmo: FuG") +
  coord_cartesian(xlim=c(-2, 2))

pdf(file="~/Documents/hiLo/plots/limbicVsFuG.pdf", width=15, height=20)
grid.arrange(p_ACgG, p_PCgG, p_AIns, p_Amygdala, p_Ent, p_Hippocampus, p_MCgG, p_PHG, p_PIns, p_SCA, p_limbic, p_FuG, nrow=4, ncol=3)
dev.off()

img.data <- img.data[, global.values]
colnames(img.data) <- c("ICV", "GMD", "MD", "CBF", "ALFF", "ReHo", "NB MFG", "Id Limbic")

corr <- cor(img.data)

p <- ggcorrplot(corr)

png("~/Documents/hiLo/plots/corrplot_color.png", width=90, height=70, units='mm', res=800)
p
dev.off()

corr2 <- round(corr, digits=3)
corr2 <- data.frame(corr2)
colnames(corr2) <- c("ICV", "GMD", "MD", "CBF", "ALFF", "ReHo", "NB MFG", "Id Limbic")


#### Get Bonferroni-corrected p

# Number of comparisons: 28
for (i in 1:ncol(img.data)) {
  for (j in 1:ncol(img.data)) {
    if (j < i) {
      pv <- (cor.test(img.data[,colnames(img.data)[i]], img.data[,colnames(img.data)[j]])$p.value)*28
      if (pv < .001) {
        sig <- "***"
        corr2[i, j] <- paste0(corr2[i, j], sig)
      } else if (pv < .01) {
        sig <- "**"
        corr2[i, j] <- paste0(corr2[i, j], sig)
      } else if (pv < .05) {
        sig <- "*"
        corr2[i, j] <- paste0(corr2[i, j], sig)
      }
    }
  }
}


write.csv(corr2, "~/Documents/hiLo/tables/globalCorr.csv")
