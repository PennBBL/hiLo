### This script runs the cross-validated ridge regressions in attempt to
### reproduce the effects in Figure 5 in the first submission
###
### Ellyn Butler
### February 26, 2020

set.seed(20)

# Source my functions
source("~/Documents/ButlerPlotFuncs/plotFuncs.R")

# Load libraries
install_load('glmnet', 'ggplot2', 'reshape2', 'gridExtra', 'psych', 'dplyr',
  'caret', 'dfoptim', 'Rmisc', 'permute')

# Load the non-age-regressed cognitive data
cog.data <- read.csv("~/Documents/hiLo/data/cognitive/n1601_cnb_factor_scores_tymoore_20151006.csv")

# Load the data
vol.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
vol.data <- merge(vol.data, cog.data, by="bblid")
vol.data <- vol.data[!is.na(vol.data$F1_Exec_Comp_Res_Accuracy),]
rownames(vol.data) <- 1:nrow(vol.data)

cbf.data <- read.csv('~/Documents/hiLo/data/meanLR/cbfData.csv')
cbf.data <- merge(cbf.data, cog.data, by="bblid")
cbf.data <- cbf.data[!is.na(cbf.data$F1_Exec_Comp_Res_Accuracy),]
rownames(cbf.data) <- 1:nrow(cbf.data)

gmd.data <- read.csv('~/Documents/hiLo/data/meanLR/gmdData.csv')
gmd.data <- merge(gmd.data, cog.data, by="bblid")
gmd.data <- gmd.data[!is.na(gmd.data$F1_Exec_Comp_Res_Accuracy),]
rownames(gmd.data) <- 1:nrow(gmd.data)

reho.data <- read.csv('~/Documents/hiLo/data/meanLR/rehoData.csv')
reho.data <- merge(reho.data, cog.data, by="bblid")
reho.data <- reho.data[!is.na(reho.data$F1_Exec_Comp_Res_Accuracy),]
rownames(reho.data) <- 1:nrow(reho.data)

alff.data <- read.csv('~/Documents/hiLo/data/meanLR/alffData.csv')
alff.data <- merge(alff.data, cog.data, by="bblid")
alff.data <- alff.data[!is.na(alff.data$F1_Exec_Comp_Res_Accuracy),]
rownames(alff.data) <- 1:nrow(alff.data)

md.data <- read.csv('~/Documents/hiLo/data/meanLR/jlfTRData.csv')
md.data <- merge(md.data, cog.data, by="bblid")
md.data <- md.data[!is.na(md.data$F1_Exec_Comp_Res_Accuracy),]
rownames(md.data) <- 1:nrow(md.data)

nback.data <- read.csv('~/Documents/hiLo/data/meanLR/nbackData.csv')
nback.data <- merge(nback.data, cog.data, by="bblid")
nback.data <- nback.data[!is.na(nback.data$F1_Exec_Comp_Res_Accuracy),]
rownames(nback.data) <- 1:nrow(nback.data)

idemo.data <- read.csv('~/Documents/hiLo/data/meanLR/idemoData.csv')
idemo.data <- merge(idemo.data, cog.data, by="bblid")
idemo.data <- idemo.data[!is.na(idemo.data$F1_Exec_Comp_Res_Accuracy),]
rownames(idemo.data) <- 1:nrow(idemo.data)

nback.data <- nback.data[!is.na(nback.data$sigchange_contrast4_2back0back_mean_miccai_ave_Accumbens_Area),]
rownames(nback.data) <- 1:nrow(nback.data)

idemo.data <- idemo.data[!is.na(idemo.data$sigchange_cope1_task_mean_miccai_ave_Accumbens_Area),]
rownames(idemo.data) <- 1:nrow(idemo.data)

all.data <- Reduce(function(x, y, ...) merge(x, y, ...),
  list(vol.data, cbf.data, gmd.data, reho.data, alff.data, md.data, nback.data, idemo.data))

dataframes <- c("vol.data", "gmd.data", "md.data", "cbf.data", "alff.data",
  "reho.data", "nback.data", "idemo.data", "all.data")

# yvar
yvar <- "F1_Exec_Comp_Res_Accuracy"

# Create a dataframe for the results
# 9 modalities, 2 perm statuses, 2 sexes, 1000 runs
results_df <- data.frame(matrix(NA, nrow=36000, ncol=5))
colnames(results_df) <- c("Modality", "Sex", "Permuted", "Run", "RSq")

results_df$Modality <- c(rep("Volume", 4000), rep("GMD", 4000), rep("MD", 4000),
  rep("CBF", 4000), rep("ALFF", 4000), rep("ReHo", 4000), rep("NBack", 4000),
  rep("IdEmo", 4000), rep("All", 4000))
results_df$Sex <- rep(c(rep("Female", 2000), rep("Male", 2000)), 9)
results_df$Permuted <- rep(c(rep("No", 1000), rep("Yes", 1000)), 18)
results_df$Run <- rep(1:1000, 36)

grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_',
  'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_',
  'sigchange_cope1_task_mean_miccai_ave_')

qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "nbackRelMeanRMSMotion", "idemoRelMeanRMSMotion")
colstouse <- c("mprage_antsCT_vol_TBV", "mprage_jlf_gmd_MeanGMD", "dti_jlf_tr_MeanTR", "pcaslMeanGMValue")

# Get RSq values for permuted and true
k=1
a=1
for (dafr in c("vol.data", "gmd.data", "md.data", "cbf.data")) {     #dataframes) {
  print(dafr)
  for (sex in c(2, 1)) {
    print(sex)
    for (perm in c("No", "Yes")) {
      print(perm)
      for (run in 1:1000) {
        thisdf <- get(dafr)
        thisdf <- thisdf[thisdf$sex == sex, ]
        rownames(thisdf) <- 1:nrow(thisdf)
        if (a %in% 1:8) {
          xvars <- colstouse[a]
        } else {
          xvars <- colstouse
        }

        # Create age vars
        thisdf$age <- scale(thisdf$ageAtGo1Scan)
        thisdf$age2 <- scale((thisdf$age)^2)
        thisdf$age3 <- scale((thisdf$age)^3)

        # Permute the cognitive variable with the age and QA variables to create a stronger null
        if (perm == "Yes") {
          reorderrows <- sample(1:nrow(thisdf), replace=FALSE)
          thisdf$F1_Exec_Comp_Res_Accuracy <- thisdf$F1_Exec_Comp_Res_Accuracy[reorderrows]
          thisdf$age <- thisdf$age[reorderrows]
          thisdf$age2 <- thisdf$age2[reorderrows]
          thisdf$age3 <- thisdf$age3[reorderrows]
          if (a < 9) {
            thisdf[, qualitymetrics[a]] <- thisdf[reorderrows, qualitymetrics[a]]
          } else {
            for (qual in qualitymetrics) {
              thisdf[, qual] <- thisdf[reorderrows, qual]
            }
          }
        }
        rownames(thisdf) <- 1:nrow(thisdf)

        # Version with half and half
        folds2 <- createFolds(thisdf$F1_Exec_Comp_Res_Accuracy, k = 2, list = TRUE)
        test <- folds2[[1]]
        train <- folds2[[2]]

        if (a < 9) {
          x_train_df <- thisdf[train, c("bblid", "age", "age2", "age3", qualitymetrics[a], xvars)]
          y_train_df <- thisdf[train, c("bblid", yvar)]
          x_test_df <- thisdf[test, c("bblid", "age", "age2", "age3", qualitymetrics[a], xvars)]
          y_test_df <- thisdf[test, c("bblid", yvar)]
          x_train_input <- as.matrix(x_train_df[, c("age", "age2", "age3", qualitymetrics[a], xvars)])
          F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy
        } else {
          x_train_df <- thisdf[train, c("bblid", "age", "age2", "age3", unique(qualitymetrics), xvars)]
          y_train_df <- thisdf[train, c("bblid", yvar)]
          x_test_df <- thisdf[test, c("bblid", "age", "age2", "age3", unique(qualitymetrics), xvars)]
          y_test_df <- thisdf[test, c("bblid", yvar)]
          x_train_input <- as.matrix(x_train_df[, c("age", "age2", "age3", unique(qualitymetrics), xvars)])
          F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy
        }

        #### Build the ridge model using only age, QA variables, and imaging variables
        ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
          lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5)
        		# https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
        lambda_cv <- ridge_model$lambda.min

        if (a < 9) {
          model_cv <- glmnet(as.matrix(x_train_df[, c("age", "age2", "age3", qualitymetrics[a], xvars)]),
            F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
          y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c("age", "age2", "age3", qualitymetrics[a], xvars)]))
        } else {
          model_cv <- glmnet(as.matrix(x_train_df[, c("age", "age2", "age3", unique(qualitymetrics), xvars)]),
            F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
          y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c("age", "age2", "age3", unique(qualitymetrics), xvars)]))
        }

        # Calculate the RSq change value
        SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
        SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
        results_df[k, "RSq"] <- 1 - SSres/SStot

        k=k+1
      }
    }
  }
  a=a+1
}

results_df$Modality <- ordered(results_df$Modality, levels=c("Volume", "GMD", "MD",
  "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "All"))

write.csv(results_df, file="~/Documents/hiLo/data/permutationResults_half_justSummary_strongNull.csv", row.names=FALSE)

toPlotVals <- summarySE(data=results_df[,c('Modality', 'Sex', 'Permuted', "RSq")],
  groupvars=c('Modality', 'Sex', 'Permuted'), measurevar='RSq')

write.csv(toPlotVals, file="~/Documents/hiLo/data/permutationSummary_half_justSummary_strongNull.csv", row.names=FALSE)

out.plot <- ggplot(results_df, aes(x=RSq, group=Permuted, fill=Permuted)) +
  geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Male",], fill="black", adjust=10) +
  geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Male",], fill="steelblue2", alpha=.5, adjust=1.5) +
  geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Female",], fill="black", adjust=10) +
  geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Female",], fill="violetred1", alpha=.5, adjust=1.5) +
  theme_linedraw() +
  facet_grid(Modality ~ Sex) +
  coord_cartesian(ylim=c(0,25),xlim=c(-.1,.5)) +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
  ylab("") +
  geom_vline(data = toPlotVals[toPlotVals$Permuted == "Yes" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Permuted == "No" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="violetred1") +
  geom_vline(data = toPlotVals[toPlotVals$Permuted == "Yes" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Permuted == "No" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="steelblue2")


png(file="~/Documents/hiLo/plots/figure7_color_justSummary_strongNull.png", height=160, width=120, units='mm', res=800)
out.plot
dev.off()
