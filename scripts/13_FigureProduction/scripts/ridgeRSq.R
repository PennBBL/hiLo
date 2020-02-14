### This script runs the cross-validated ridge regressions in attempt to
### reproduce the effects in Figure 5 in the first submission
###
### Ellyn Butler
### February 11, 2020 - February 14, 2020

# Source my functions
source("~/Documents/ButlerPlotFuncs/plotFuncs.R")

# Load libraries
install_load('glmnet', 'ggplot2', 'reshape2', 'gridExtra', 'psych', 'dplyr',
  'caret', 'dfoptim', 'Rmisc', 'permute')

# Load the data
vol.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
cbf.data <- read.csv('~/Documents/hiLo/data/meanLR/cbfData.csv')
gmd.data <- read.csv('~/Documents/hiLo/data/meanLR/gmdData.csv')
reho.data <- read.csv('~/Documents/hiLo/data/meanLR/rehoData.csv')
alff.data <- read.csv('~/Documents/hiLo/data/meanLR/alffData.csv')
md.data <- read.csv('~/Documents/hiLo/data/meanLR/jlfTRData.csv')
nback.data <- read.csv('~/Documents/hiLo/data/meanLR/nbackData.csv')
idemo.data <- read.csv('~/Documents/hiLo/data/meanLR/idemoData.csv')

nback.data <- nback.data[!is.na(nback.data$sigchange_contrast4_2back0back_mean_miccai_ave_Accumbens_Area),]
rownames(nback.data) <- 1:nrow(nback.data)
idemo.data <- idemo.data[!is.na(idemo.data$sigchange_cope1_task_mean_miccai_ave_Accumbens_Area),]
rownames(idemo.data) <- 1:nrow(idemo.data)

all.data <- Reduce(function(x, y, ...) merge(x, y, ...),
  list(vol.data, cbf.data, gmd.data, reho.data, alff.data, md.data, nback.data, idemo.data))

dataframes <- c("vol.data", "gmd.data", "md.data", "cbf.data", "alff.data",
  "reho.data", "nback.data", "idemo.data", "all.data")

# yvar
yvar <- "F1_Exec_Comp_Cog_Accuracy"

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

# Get RSq values for permuted and true
k=1
a=1
for (dafr in dataframes) {
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

        if (perm == "Yes") {
          thisdf$F1_Exec_Comp_Cog_Accuracy <- sample(thisdf$F1_Exec_Comp_Cog_Accuracy, replace=FALSE)
        }

        folds10 <- createFolds(thisdf$F1_Exec_Comp_Cog_Accuracy, k = 10, list = TRUE)

        ### Create training and testing dataframes
        test <- folds10[[10]]
        trainfolds <- subset(folds10, !(grepl(10, names(folds10))))
        train <- c()
        for (j in 1:9) { train <- c(train, trainfolds[[j]]) }

        x_train_df <- thisdf[train, c("bblid", xvars)]
        x_train_input <- as.matrix(x_train_df[,xvars])
        y_train_df <- thisdf[train, c("bblid", yvar)]
        F1_Exec_Comp_Cog_Accuracy <- y_train_df$F1_Exec_Comp_Cog_Accuracy

        ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Cog_Accuracy, alpha=0,
          lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5)
        		# https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
        assign(paste0("mod_", i), elastic_net_model)
        x_test_df <- thisdf[test, c("bblid", xvars)]
        y_test_df <- thisdf[test, c("bblid", yvar)]
        lambda_cv <- ridge_model$lambda.min
        model_cv <- glmnet(as.matrix(x_train_df[,xvars]), F1_Exec_Comp_Cog_Accuracy, alpha = 0, lambda = lambda_cv, standardize = TRUE)

        y_test_predicted <- predict(model_cv, as.matrix(x_test_df[,xvars]))

        # Calculate the RSq value
        SSres <- sum((y_test_df$F1_Exec_Comp_Cog_Accuracy - y_test_predicted)^2)
        SStot <- sum((y_test_df$F1_Exec_Comp_Cog_Accuracy - mean(y_train_df$F1_Exec_Comp_Cog_Accuracy))^2)
        results_df[k, "RSq"] <- 1 - SSres/SStot
        k=k+1
      }
    }
  }
  a=a+1
}

results_df$Modality <- ordered(results_df$Modality, levels=c("Volume", "GMD", "MD",
  "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "All"))

write.csv(results_df, file="~/Documents/hiLo/data/permutationResults.csv", row.names=FALSE)

toPlotVals <- summarySE(data=results_df[,c('Modality', 'Sex', 'Permuted', "RSq")],
  groupvars=c('Modality', 'Sex', 'Permuted'), measurevar='RSq')

write.csv(toPlotVals, file="~/Documents/hiLo/data/permutationSummary.csv", row.names=FALSE)

out.plot <- ggplot(results_df, aes(x=RSq, group=Permuted, fill=Permuted)) +
  geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Male",], fill="steelblue2") +
  #geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Male",], fill="black") +
  geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Female",], fill="violetred1") +
  #geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Female",], fill="black") +
  theme_linedraw() +
  facet_grid(Modality ~ Sex) +
  #coord_cartesian(ylim=c(0,500),xlim=c(-.1,.5)) +
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



png(file="~/Documents/hiLo/plots/figure7_color.png", height=160, width=120, units='mm', res=800)
out.plot
dev.off()


#boo <- results_df[results_df$Modality == "Volume" & results_df$Sex == "Female" & results_df$Permuted == "Yes",]
