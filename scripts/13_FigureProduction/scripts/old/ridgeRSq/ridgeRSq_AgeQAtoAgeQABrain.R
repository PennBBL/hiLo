### This script runs the cross-validated ridge regressions in attempt to
### reproduce the effects in Figure 5 in the first submission
###
### Ellyn Butler
### March 4, 2020 - March 11, 2020

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
colnames(results_df) <- c("Modality", "Sex", "Null", "Run", "RSq")

results_df$Modality <- c(rep("Volume", 4000), rep("GMD", 4000), rep("MD", 4000),
  rep("CBF", 4000), rep("ALFF", 4000), rep("ReHo", 4000), rep("NBack", 4000),
  rep("IdEmo", 4000), rep("All", 4000))
results_df$Sex <- rep(c(rep("Female", 2000), rep("Male", 2000)), 9)
results_df$Null <- rep(c(rep("No", 1000), rep("Yes", 1000)), 18)
results_df$Run <- rep(1:1000, 36)

grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_',
  'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_',
  'sigchange_cope1_task_mean_miccai_ave_')

qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "nbackRelMeanRMSMotion", "idemoRelMeanRMSMotion")

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
        xvars <- xvars[1:5]

        # Create age vars
        thisdf$age <- scale(thisdf$ageAtGo1Scan)
        thisdf$age2 <- scale((thisdf$age)^2)
        thisdf$age3 <- scale((thisdf$age)^3)
        rownames(thisdf) <- 1:nrow(thisdf)

        # Version with half and half
        folds2 <- createFolds(thisdf$F1_Exec_Comp_Res_Accuracy, k = 2, list = TRUE)
        test <- folds2[[1]]
        train <- folds2[[2]]

        train_df <- thisdf[train, ]
        rownames(train_df) <- 1:nrow(train_df)
        test_df <- thisdf[test, ]
        rownames(test_df) <- 1:nrow(test_df)

        if (null == "No") {
          if (a < 9) {
            xvarsfunc <- ""
            for (xv in xvars) { xvarsfunc <- paste0(xvarsfunc, xv, " + ") }
            thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", xvarsfunc, qualitymetrics[a], " + age + age2 + age3")
            ols_model <- lm(formula(thisfunc), data=train_df)
            y_test_predicted <- predict(ols_model, newdata=test_df)
          } else {
            xvarsfunc <- ""
            for (xv in xvars) { xvarsfunc <- paste0(xvarsfunc, xv, " + ") }
            qualitymetricsunique <- unique(qualitymetrics)
            qualfunc <- ""
            for (qm in qualitymetricsunique) { qualfunc <- paste0(qualfunc, qm, " + ") }
            thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", xvarsfunc, qualfunc, " age + age2 + age3")
            ols_model <- lm(formula(thisfunc), data=train_df)
            y_test_predicted <- predict(ols_model, newdata=test_df)
          }
        } else {
          if (a < 9) {
            thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", qualitymetrics[a], " + age + age2 + age3")
            ols_model <- lm(formula(thisfunc), data=train_df)
            y_test_predicted <- predict(ols_model, newdata=test_df)
          } else {
            qualitymetricsunique <- unique(qualitymetrics)
            qualfunc <- ""
            for (qm in qualitymetricsunique) { qualfunc <- paste0(qualfunc, qm, " + ") }
            thisfunc <- paste0("F1_Exec_Comp_Res_Accuracy ~ ", qualfunc, " age + age2 + age3")
            ols_model <- lm(formula(thisfunc), data=train_df)
            y_test_predicted <- predict(ols_model, newdata=test_df)
          }
        }

        # Calculate the RSq change value
        SSres <- sum((test_df$F1_Exec_Comp_Res_Accuracy - y_test_predicted)^2)
        SStot <- sum((test_df$F1_Exec_Comp_Res_Accuracy - mean(train_df$F1_Exec_Comp_Res_Accuracy))^2)
        results_df[k, "RSq"] <- 1 - SSres/SStot

        k=k+1
      }
    }
  }
  a=a+1
}

results_df$Modality <- ordered(results_df$Modality, levels=c("Volume", "GMD", "MD",
  "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "All"))

write.csv(results_df, file="~/Documents/hiLo/data/permutationResults_half_AgeQAtoAgeQABrain_OLSsubset.csv", row.names=FALSE)

toPlotVals <- summarySE(data=results_df[,c('Modality', 'Sex', 'Null', 'RSq')],
  groupvars=c('Modality', 'Sex', 'Null'), measurevar='RSq')

write.csv(toPlotVals, file="~/Documents/hiLo/data/permutationSummary_half_AgeQAtoAgeQABrain_OLSsubset.csv", row.names=FALSE)

out.plot <- ggplot(results_df, aes(x=RSq, group=Null, fill=Null)) +
  geom_density(data=results_df[results_df$Null=='Yes' & results_df$Sex=="Male",], fill="black") +
  geom_density(data=results_df[results_df$Null=='No' & results_df$Sex=="Male",], fill="steelblue2", alpha=.5) +
  geom_density(data=results_df[results_df$Null=='Yes' & results_df$Sex=="Female",], fill="black") +
  geom_density(data=results_df[results_df$Null=='No' & results_df$Sex=="Female",], fill="violetred1", alpha=.5) +
  theme_linedraw() +
  facet_grid(Modality ~ Sex) +
  coord_cartesian(ylim=c(0,25),xlim=c(-.1,.6)) +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
  ylab("") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "Yes" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "No" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="violetred1") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "Yes" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "No" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="steelblue2")

out.plot_histogram <- ggplot(results_df, aes(x=RSq, group=Null, fill=Null)) +
  geom_histogram(data=results_df[results_df$Null=='Yes' & results_df$Sex=="Male",], bins=50, fill="black") +
  geom_histogram(data=results_df[results_df$Null=='No' & results_df$Sex=="Male",], bins=50, fill="steelblue2", alpha=.5) +
  geom_histogram(data=results_df[results_df$Null=='Yes' & results_df$Sex=="Female",], bins=50, fill="black") +
  geom_histogram(data=results_df[results_df$Null=='No' & results_df$Sex=="Female",], bins=50, fill="violetred1", alpha=.5) +
  theme_linedraw() +
  facet_grid(Modality ~ Sex) +
  coord_cartesian(ylim=c(0,200),xlim=c(-.1,.6)) +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
  ylab("") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "Yes" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "No" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="violetred1") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "Yes" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Null == "No" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="steelblue2")


png(file="~/Documents/hiLo/plots/figure7_color_AgeQAtoAgeQABrain_OLSsubset.png", height=160, width=120, units='mm', res=800)
out.plot
dev.off()

png(file="~/Documents/hiLo/plots/figure7_color_AgeQAtoAgeQABrain_OLSsubset_histogram.png", height=160, width=120, units='mm', res=800)
out.plot_histogram
dev.off()
