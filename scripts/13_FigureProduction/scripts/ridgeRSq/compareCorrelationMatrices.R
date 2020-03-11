### This script compares the correlation matrices between brain features
### before and after regressing out age with the idea that if the volume
### variables become less correlated after age regression, then maybe that
### is resulting in the discrepancies between the regressing out version of the
### plot and the null/alternative version
###
### Ellyn Butler
### March 11, 2020

set.seed(20)

# Source my functions
source("~/Documents/ButlerPlotFuncs/plotFuncs.R")

# Load libraries
install_load('glmnet', 'ggplot2', 'reshape2', 'gridExtra', 'psych', 'dplyr',
  'caret', 'dfoptim', 'Rmisc', 'permute', 'ggcorrplot')

# Load the non-age-regressed cognitive data
cog.data <- read.csv("~/Documents/hiLo/data/cognitive/n1601_cnb_factor_scores_tymoore_20151006.csv")

# Load the data
vol.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
vol.data <- merge(vol.data, cog.data, by="bblid")
vol.data <- vol.data[!is.na(vol.data$F1_Exec_Comp_Res_Accuracy),]
rownames(vol.data) <- 1:nrow(vol.data)

thisdf <- merge(vol.data, cog.data)

grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_',
  'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_',
  'sigchange_cope1_task_mean_miccai_ave_')

# Create age vars
thisdf$age <- scale(thisdf$ageAtGo1Scan)
thisdf$age2 <- scale((thisdf$age)^2)
thisdf$age3 <- scale((thisdf$age)^3)

colstouse <- grep(grepVals[1], colnames(thisdf), value=TRUE)

colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
xvars <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]

qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "nbackRelMeanRMSMotion", "idemoRelMeanRMSMotion")

corr_orig <- cor(thisdf[,xvars])
p_orig <- ggcorrplot(corr_orig) + ggtitle("Original")

for (xvar in xvars) {
  thisfunc <- paste0(xvar, " ~ age + age2 + age3 + ", qualitymetrics[1])
  thisdf[,paste0(xvar, "_regressed")] <- lm(formula(thisfunc), data=thisdf)$residuals
}

corr_regress <- cor(thisdf[,paste0(xvars, "_regressed")])
p_regress <- ggcorrplot(corr_regress) + ggtitle("Regressed")

pdf(file="~/Documents/hiLo/plots/compareCorrelationMatrices.pdf", width=40, height=20)
grid.arrange(p_orig, p_regress, ncol=2)
dev.off()

# Doesn't seem like this is the source of the discrepancy... but may be a multicollinearity issue








#
