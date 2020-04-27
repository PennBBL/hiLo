### This script runs a series of regressions (OLS and ridge) to estimate
### out of sample R^2s for a variety of combinations of predictors
### SET UP FOR PMACS
###
### Ellyn Butler
### April 27, 2020

set.seed(20)

# Source my functions
source("~/ButlerPlotFuncs/plotFuncs.R")

# Load libraries
install_load('glmnet', 'ggplot2', 'reshape2', 'gridExtra', 'psych', 'dplyr',
  'caret', 'dfoptim', 'Rmisc', 'permute')

# Load the non-age-regressed cognitive data
cog.data <- read.csv("~/hiLo/data/cognitive/n1601_cnb_factor_scores_tymoore_20151006.csv")
cog.data <- cog.data[!is.na(cog.data$F1_Exec_Comp_Res_Accuracy),]

# Load the data
modalities <- c('volume', 'gmd', 'md', 'cbf', 'alff', 'reho', 'nback', 'idemo')

for (modal in modalities) {
  tmp_df <- read.csv(paste0('~/hiLo/data/meanLR/', modal, 'Data.csv'))
  tmp_df <- merge(tmp_df, cog.data, by='bblid')
  assign(paste0(modal, '.data'), tmp_df)
}

all.data <- Reduce(function(x, y, ...) merge(x, y, ...),
  list(volume.data, cbf.data, gmd.data, reho.data, alff.data, md.data, nback.data, idemo.data))

dataframes <- c(paste0(modalities, '.data'), 'all.data')

# yvar
yvar <- "F1_Exec_Comp_Res_Accuracy"

# Create a dataframe for the results
# 9 modalities, 2 perm statuses, 2 sexes, 10000 runs
numits=10000

results_df <- data.frame(matrix(NA, nrow=126*numits, ncol=5))
colnames(results_df) <- c("Modality", "Sex", "Group", "Run", "RSq")

results_df$Modality <- c(rep("Volume", 14*numits), rep("GMD", 14*numits), rep("MD", 14*numits),
  rep("CBF", 14*numits), rep("ALFF", 14*numits), rep("ReHo", 14*numits), rep("NBack", 14*numits),
  rep("IdEmo", 14*numits), rep("All", 14*numits))
results_df$Sex <- rep(c(rep("Female", 7*numits), rep("Male", 7*numits)), 9)
results_df$Group <- rep(c(rep("QA", numits), rep("QABrain", numits), rep("AgeQA", numits),
  rep("AgeQABrain", numits), rep("Age", numits), rep("AgeBrain", numits), rep("Brain", numits)), 18)
results_df$Run <- rep(1:numits, 126)

grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_',
  'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_',
  'sigchange_cope1_task_mean_miccai_ave_')

qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "nbackRelMeanRMSMotion", "idemoRelMeanRMSMotion")

# Get RSq values for all combinations of predictors
k=1
a=1
###############
for (dafr in dataframes) {
  for (sex in c(2, 1)) {
    thisdf <- get(dafr)
    thisdf <- thisdf[thisdf$sex == sex, ]
    rownames(thisdf) <- 1:nrow(thisdf)
    if (a %in% 1:8) {
      colstouse <- grep(grepVals[a], colnames(thisdf), value=TRUE)
    } else {
      colstouse <- c(grep(grepVals[1], colnames(thisdf), value=TRUE),
        grep(grepVals[2], colnames(thisdf), value=TRUE), grep(grepVals[3], colnames(thisdf), value=TRUE),
        grep(grepVals[4], colnames(thisdf), value=TRUE), grep(grepVals[5], colnames(thisdf), value=TRUE),
        grep(grepVals[6], colnames(thisdf), value=TRUE), grep(grepVals[7], colnames(thisdf), value=TRUE),
        grep(grepVals[8], colnames(thisdf), value=TRUE))
    }
    colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
    colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
    colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
    colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
    colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
    xvars <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]

    # Create age vars
    thisdf$age <- scale(thisdf$ageAtGo1Scan)
    thisdf$age2 <- scale((thisdf$age)^2)
    thisdf$age3 <- scale((thisdf$age)^3)
    rownames(thisdf) <- 1:nrow(thisdf)

    ###############
    for (run in 1:numits) {
      # Version with half and half
      folds2 <- createFolds(thisdf$F1_Exec_Comp_Res_Accuracy, k = 2, list = TRUE)
      test <- folds2[[1]]
      train <- folds2[[2]]

      if (a < 9) {
        train_df <- thisdf[train, ]
        x_train_df <- thisdf[train, c("bblid", "age", "age2", "age3", qualitymetrics[a], xvars)]
        y_train_df <- thisdf[train, c("bblid", yvar)]
        x_test_df <- thisdf[test, c("bblid", "age", "age2", "age3", qualitymetrics[a], xvars)]
        y_test_df <- thisdf[test, c("bblid", yvar)]
      } else {
        train_df <- thisdf[train, ]
        x_train_df <- thisdf[train, c("bblid", "age", "age2", "age3", unique(qualitymetrics), xvars)]
        y_train_df <- thisdf[train, c("bblid", yvar)]
        x_test_df <- thisdf[test, c("bblid", "age", "age2", "age3", unique(qualitymetrics), xvars)]
        y_test_df <- thisdf[test, c("bblid", yvar)]
      }

      ###### QA ###### k
      if (a < 9) {
        thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", qualitymetrics[a])
        ols_model <- lm(formula(thisfunc), data=train_df)
        y_test_predicted <- predict(ols_model, newdata=x_test_df)
      } else {
        qualitymetricsunique <- unique(qualitymetrics)
        qualfunc <- ""
        j=1
        for (qm in qualitymetricsunique) {
          if (j < length(qualitymetricsunique)) {
            qualfunc <- paste0(qualfunc, qm, " + ")
          } else { qualfunc <- paste0(qualfunc, qm) }
          j=j+1
        }
        thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", qualfunc)
        ols_model <- lm(formula(thisfunc), data=train_df)
        y_test_predicted <- predict(ols_model, newdata=x_test_df)
      }
      # Calculate RSq
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k, "RSq"] <- 1 - SSres/SStot

      ###### QABrain ###### k + numits
      if (a < 9) {
        x_train_input <- as.matrix(x_train_df[, c(qualitymetrics[a], xvars)])
        F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy

        #### Build the ridge model using only age, QA variables, and imaging variables
        # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
        ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
          lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:4)
        lambda_cv <- ridge_model$lambda.min
        model_cv <- glmnet(as.matrix(x_train_df[, c(qualitymetrics[a], xvars)]),
          F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
        y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c(qualitymetrics[a], xvars)]))
      } else {
        x_train_input <- as.matrix(x_train_df[, c(unique(qualitymetrics), xvars)])
        F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy
        #### Build the ridge model using only QA variables, and imaging variables

        # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
        ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
          lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:(length(unique(qualitymetrics))))
        lambda_cv <- ridge_model$lambda.min
        model_cv <- glmnet(as.matrix(x_train_df[, c(unique(qualitymetrics), xvars)]),
          F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
        y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c(unique(qualitymetrics), xvars)]))
      }
      # Calculate RSq
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k + numits, "RSq"] <- 1 - SSres/SStot

      ###### AgeQA ###### k + 2*numits
      if (a < 9) {
        thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", qualitymetrics[a], " + age + age2 + age3")
        ols_model <- lm(formula(thisfunc), data=train_df)
        y_test_predicted <- predict(ols_model, newdata=x_test_df)
      } else {
        qualitymetricsunique <- unique(qualitymetrics)
        qualfunc <- ""
        for (qm in qualitymetricsunique) { qualfunc <- paste0(qualfunc, qm, " + ") }
        thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", qualfunc, " age + age2 + age3")
        ols_model <- lm(formula(thisfunc), data=train_df)
        y_test_predicted <- predict(ols_model, newdata=x_test_df)
      }
      # Calculate RSq
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k + 2*numits, "RSq"] <- 1 - SSres/SStot

      ###### AgeQABrain ###### k + 3*numits
      if (a < 9) {
        x_train_input <- as.matrix(x_train_df[, c("age", "age2", "age3", qualitymetrics[a], xvars)])
        F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy

        #### Build the ridge model using only age, QA variables, and imaging variables
        # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
        ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
          lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:4)
        lambda_cv <- ridge_model$lambda.min
        model_cv <- glmnet(as.matrix(x_train_df[, c("age", "age2", "age3", qualitymetrics[a], xvars)]),
          F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
        y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c("age", "age2", "age3", qualitymetrics[a], xvars)]))
      } else {
        x_train_input <- as.matrix(x_train_df[, c("age", "age2", "age3", unique(qualitymetrics), xvars)])
        F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy
        #### Build the ridge model using only age, QA variables, and imaging variables

        # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
        ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
          lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:(3+length(unique(qualitymetrics))))
        lambda_cv <- ridge_model$lambda.min
        model_cv <- glmnet(as.matrix(x_train_df[, c("age", "age2", "age3", unique(qualitymetrics), xvars)]),
          F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
        y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c("age", "age2", "age3", unique(qualitymetrics), xvars)]))
      }
      # Calculate RSq
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k + 3*numits, "RSq"] <- 1 - SSres/SStot

      ###### Age ###### k + 4*numits
      thisfunc <- "F1_Exec_Comp_Res_Accuracy ~ age + age2 + age3"
      ols_model <- lm(formula(thisfunc), data=train_df)
      y_test_predicted <- predict(ols_model, newdata=x_test_df)
      # Calculate the RSq change value
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k + 4*numits, "RSq"] <- 1 - SSres/SStot

      ###### AgeBrain ###### k + 5*numits
      x_train_input <- as.matrix(x_train_df[, c("age", "age2", "age3", xvars)])
      F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy
      #### Build the ridge model using only age, QA variables, and imaging variables

      # ------------ PENALIZE ONLY BRAIN VARIABLES ----------- #
      ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
        lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5, exclude=1:3)
      lambda_cv <- ridge_model$lambda.min
      model_cv <- glmnet(as.matrix(x_train_df[, c("age", "age2", "age3", xvars)]),
        F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
      y_test_predicted <- predict(model_cv, as.matrix(x_test_df[, c("age", "age2", "age3", xvars)]))
      # Calculate the RSq change value
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k + 5*numits, "RSq"] <- 1 - SSres/SStot

      ###### Brain ###### k + 6*numits
      x_train_input <- as.matrix(x_train_df[,xvars])
      F1_Exec_Comp_Res_Accuracy <- y_train_df$F1_Exec_Comp_Res_Accuracy

      # Build the ridge model
      ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Res_Accuracy, alpha=0,
        lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5)
      lambda_cv <- ridge_model$lambda.min
      model_cv <- glmnet(as.matrix(x_train_df[,xvars]), F1_Exec_Comp_Res_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)
      y_test_predicted <- predict(model_cv, as.matrix(x_test_df[,xvars]))

      # Calculate the RSq value
      SSres <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
      SStot <- sum((y_test_df$F1_Exec_Comp_Res_Accuracy - mean(y_train_df$F1_Exec_Comp_Res_Accuracy))^2)
      results_df[k + 6*numits, "RSq"] <- 1 - SSres/SStot

      k=k+1
    }
    k=k+6*numits
  }
  a=a+1
}

results_df$Modality <- ordered(results_df$Modality, levels=c("Volume", "GMD", "MD",
  "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "All"))

write.csv(results_df, file="~/hiLo/data/r2results/results_ridgeRSq_combined_tenthous.csv", row.names=FALSE)

toPlotVals <- summarySE(data=results_df[,c('Modality', 'Sex', 'Group', 'RSq')],
  groupvars=c('Modality', 'Sex', 'Group'), measurevar='RSq')

write.csv(toPlotVals, file="~/hiLo/data/r2results/summary_ridgeRSq_combined_tenthous.csv", row.names=FALSE)


##########################################

# Create a table of proportions age+brain less than age alone
# Modality, Sex, Prop, MultCompProp (have to decide how to do this)

prop_df <- tibble(Modality=rep(levels(results_df$Modality), 2),
  Sex=c(rep("Female", 9), rep("Male", 9)), Prop=rep(NA, 18),
  mean_Age_rsq=rep(NA, 18), mean_AgeBrain_rsq=rep(NA, 18),
  mean_AgeBrain_minus_Age_rsq=rep(NA, 18))

i=1
for (sex in c("Female", "Male")) {
  for (mod in levels(results_df$Modality)) {
    prop_df[i, "Prop"] <- sum((results_df[results_df$Sex == sex &
      results_df$Modality == mod & results_df$Group == "AgeBrain", "RSq"]) <
      results_df[results_df$Sex == sex & results_df$Modality == mod &
      results_df$Group == "Age", "RSq"])/numits
    prop_df[i, "mean_Age_rsq"] <- mean(results_df[results_df$Sex == sex &
      results_df$Modality == mod & results_df$Group == "Age", "RSq"])
    prop_df[i, "mean_AgeBrain_rsq"] <- mean(results_df[results_df$Sex == sex &
      results_df$Modality == mod & results_df$Group == "AgeBrain", "RSq"])
    prop_df[i, "mean_AgeBrain_minus_Age_rsq"] <- prop_df[i, "mean_AgeBrain_rsq"] - prop_df[i, "mean_Age_rsq"]
    i=i+1
  }
}

prop_df$MultiCompProp <- p.adjust(prop_df$Prop, method="fdr")

write.csv(prop_df, file="~/hiLo/data/r2results/age_versus_agebrain_tenthous.csv", row.names=FALSE)























#


















#
