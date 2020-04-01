### This script runs the cross-validated ridge regressions in attempt to
### reproduce the effects in Figure 5 in the first submission
###
### Ellyn Butler
### March 31, 2020

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

grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_',
  'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_',
  'sigchange_cope1_task_mean_miccai_ave_')

# Create a dataframe for the results
# 9 modalities, 2 perm statuses, 2 sexes, 1000 runs
results_df <- data.frame(matrix(NA, nrow=36000, ncol=5))
colnames(results_df) <- c("Modality", "Sex", "Null", "Run", "RSq")

results_df$Modality <- c(rep("Volume", 4000), rep("GMD", 4000), rep("MD", 4000),
  rep("CBF", 4000), rep("ALFF", 4000), rep("ReHo", 4000), rep("NBack", 4000),
  rep("IdEmo", 4000), rep("All", 4000))
results_df$Sex <- rep(c(rep("Female", 2000), rep("Male", 2000)), 9)
results_df$Null <- rep(c(rep("No", 1000), rep("Yes", 1000)), 18)
results_df$Run <- rep(1:1000, 36)

# Get RSq values for Null and true
k=1
a=1
for (dafr in dataframes) {
  print(dafr)
  for (sex in c(2, 1)) {
    print(sex)
    for (null in c("No", "Yes")) {
      print(null)
      for (run in 1:1000) {
        thisdf <- get(dafr)
        thisdf <- thisdf[thisdf$sex == sex, ]
        row.names(thisdf) <- 1:nrow(thisdf)
        if (a %in% 1:8) {
          colstouse <- grep(grepVals[a], names(thisdf), value=TRUE)
        } else {
          colstouse <- c(grep(grepVals[1], names(thisdf), value=TRUE),
            grep(grepVals[2], names(thisdf), value=TRUE), grep(grepVals[3], names(thisdf), value=TRUE),
            grep(grepVals[4], names(thisdf), value=TRUE), grep(grepVals[5], names(thisdf), value=TRUE),
            grep(grepVals[6], names(thisdf), value=TRUE), grep(grepVals[7], names(thisdf), value=TRUE),
            grep(grepVals[8], names(thisdf), value=TRUE))
        }
        colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
        colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
        colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
        colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
        colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
        xvars <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]

        # Create the bootstrapped sample
        bootrows <- sample(row.names(thisdf), nrow(thisdf), replace=TRUE)
        thisdf <- thisdf[bootrows, ]
        row.names(thisdf) <- 1:nrow(thisdf)

        # Create age vars
        thisdf$age <- scale(thisdf$ageAtGo1Scan)
        thisdf$age2 <- scale((thisdf$age)^2)
        thisdf$age3 <- scale((thisdf$age)^3)
        rownames(thisdf) <- 1:nrow(thisdf)

        x_df <- thisdf[, c("bblid", "age", "age2", "age3", xvars)]
        y_df <- thisdf[, c("bblid", yvar)]

        if (null == "No") {
          if (a < 9) {
            x_input <- as.matrix(x_df[, c("age", "age2", "age3", xvars)])
            F1_Exec_Comp_Res_Accuracy <- y_df$F1_Exec_Comp_Res_Accuracy

            #### Build the ridge model using only age and imaging variables
            # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
            ridge_model <- cv.glmnet(x_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
            lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:4)
            lambda_cv <- ridge_model$lambda.min
            model_cv <- glmnet(as.matrix(x_df[, c("age", "age2", "age3", xvars)]),
              F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
          } else {
            x_train_input <- as.matrix(x_train_df[, c("age", "age2", "age3", xvars)])
            F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy
            #### Build the ridge model using only age and imaging variables

            # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
            ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
              lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:3)
                # https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
            lambda_cv <- ridge_model$lambda.min
            model_cv <- glmnet(as.matrix(x_train_df[, c("age", "age2", "age3", xvars)]),
              F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
          }
        } else {
          thisfunc <- "F1_Exec_Comp_Res_Accuracy ~ age + age2 + age3"
          ols_model <- lm(formula(thisfunc), data=train_df)
        }

        # Try calculating individual R^2 values
        SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
        SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
        results_df[k, "RSq"] <- 1 - SSres/SStot

        k=k+1
      }
    }
  }
  a=a+1
}



##############################################################################
# Create a dataframe for anova results
#anova_df <- data.frame(matrix(NA, nrow=18, ncol=4))
#names(anova_df) <- c("Modality", "Sex", "ChiSq", "Sig")

#anova_df$Modality <- c(rep("Volume", 2), rep("GMD", 2), rep("MD", 2),
#  rep("CBF", 2), rep("ALFF", 2), rep("ReHo", 2), rep("NBack", 2),
#  rep("IdEmo", 2), rep("All", 2))
#anova_df$Sex <- rep(c("Female", "Male"), 9)

#k=1
#a=1
#for (dafr in dataframes) {
#  print(dafr)
#  for (sex in c(2, 1)) {
#    print(sex)
#    thisdf <- get(dafr)
#    thisdf <- thisdf[thisdf$sex == sex, ]
#    row.names(thisdf) <- 1:nrow(thisdf)
#    if (a %in% 1:8) {
#      colstouse <- grep(grepVals[a], names(thisdf), value=TRUE)
#    } else {
#      colstouse <- c(grep(grepVals[1], names(thisdf), value=TRUE),
#        grep(grepVals[2], names(thisdf), value=TRUE), grep(grepVals[3], names(thisdf), value=TRUE),
#        grep(grepVals[4], names(thisdf), value=TRUE), grep(grepVals[5], names(thisdf), value=TRUE),
#        grep(grepVals[6], names(thisdf), value=TRUE), grep(grepVals[7], names(thisdf), value=TRUE),
#        grep(grepVals[8], names(thisdf), value=TRUE))
#    }
#    colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
#    colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
#    colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
#    colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
#    colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
#    xvars <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]

#    # Create age vars
#    thisdf$age <- scale(thisdf$ageAtGo1Scan)
#    thisdf$age2 <- scale((thisdf$age)^2)
#    thisdf$age3 <- scale((thisdf$age)^3)
#    rownames(thisdf) <- 1:nrow(thisdf)

#    x_df <- thisdf[, c("bblid", "age", "age2", "age3", xvars)]
#    y_df <- thisdf[, c("bblid", yvar)]

#    if (a < 9) {
#      x_input <- as.matrix(x_df[, c("age", "age2", "age3", xvars)])
#      F1_Exec_Comp_Res_Accuracy <- y_df$F1_Exec_Comp_Res_Accuracy

#      #### Build the ridge model using only age and imaging variables
#      # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
#      ridge_model <- cv.glmnet(x_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
#      lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:4)
#      lambda_cv <- ridge_model$lambda.min
#      model_cv <- glmnet(as.matrix(x_df[, c("age", "age2", "age3", xvars)]),
#        F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
#    } else {
#      x_input <- as.matrix(x_df[, c("age", "age2", "age3", xvars)])
#      F1_Exec_Comp_Res_Accuracy <- y_df$F1_Exec_Comp_Res_Accuracy
#      #### Build the ridge model using only age and imaging variables
#
#      # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
#      ridge_model <- cv.glmnet(x_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
#        lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:3)
#      lambda_cv <- ridge_model$lambda.min
#      model_cv <- glmnet(as.matrix(x_df[, c("age", "age2", "age3", xvars)]),
#        F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
#    }

#    thisfunc <- "F1_Exec_Comp_Res_Accuracy ~ age + age2 + age3"
#    ols_model <- lm(formula(thisfunc), data=train_df)

    # Calculate anova
#    k=k+1
#  }
#  a=a+1
#}








##############################################################################


write.csv(results_df, file="~/Documents/hiLo/data/results_bootstrap_OLSNoPenalizeAge.csv", row.names=FALSE)
