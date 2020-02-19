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
# 9 modalities, 2 perm statuses, 2 sexes, 500 runs
results_df <- data.frame(matrix(NA, nrow=18000, ncol=8))
colnames(results_df) <- c("Modality", "Sex", "Permuted", "Run", "RSq_10", "RSq_5", "RSq_2", "RSq_1")

results_df$Modality <- c(rep("Volume", 2000), rep("GMD", 2000), rep("MD", 2000),
  rep("CBF", 2000), rep("ALFF", 2000), rep("ReHo", 2000), rep("NBack", 2000),
  rep("IdEmo", 2000), rep("All", 2000))
results_df$Sex <- rep(c(rep("Female", 1000), rep("Male", 1000)), 9)
results_df$Permuted <- rep(c(rep("No", 500), rep("Yes", 500)), 18)
results_df$Run <- rep(1:500, 36)

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
      for (run in 1:500) {
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
        for (i in 1:10) {
          if (i < 10) { l <- paste0(0, i) }
          test <- folds10[[i]]
          trainfolds <- subset(folds10, !(grepl(l, names(folds10))))
          train <- c()
        	for (j in 1:9) { train <- c(train, trainfolds[[j]]) }
          assign(paste0("x_train_", i), thisdf[train, c("bblid", xvars)])
          assign(paste0("x_test_", i), thisdf[test, c("bblid", xvars)])
          assign(paste0("y_train_", i), thisdf[train, c("bblid", yvar)])
          assign(paste0("y_test_", i), thisdf[test, c("bblid", yvar)])

        	x_train_df <- get(paste0("x_train_", i))
        	x_train_input <- as.matrix(x_train_df[,xvars])
        	y_train_df <- get(paste0("y_train_", i))
        	F1_Exec_Comp_Cog_Accuracy <- y_train_df$F1_Exec_Comp_Cog_Accuracy

          #ridge_model <- cv.glmnet(x_train_input, F1_Exec_Comp_Cog_Accuracy, alpha=0,
            #lambda=10^seq(-3, 5, length.out = 100), standardize=TRUE, nfolds=5)
        		# just used parameters from here: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
        	x_test_df <- get(paste0("x_test_", i))
        	y_test_df <- get(paste0("y_test_", i))
          #lambda_cv <- ridge_model$lambda.min
          model_cv <- glmnet(as.matrix(x_train_df[,xvars]), F1_Exec_Comp_Cog_Accuracy, alpha = 0, lambda = .00001, standardize = TRUE)

        	y_test_predicted <- predict(model_cv, as.matrix(x_test_df[,xvars]))
        	if (i == 1) {
        		predicted_values <- merge(cbind(x_test_df, y_test_predicted), y_test_df)
            # Calculate the RSq value... weird way Adon did it...
            SSres <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - predicted_values$s0)^2)
            SStot <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - mean(predicted_values$F1_Exec_Comp_Cog_Accuracy))^2)
            results_df[k, "RSq_1"] <- 1 - SSres/SStot
        	} else {
        		predicted_values <- rbind(predicted_values, merge(cbind(x_test_df, y_test_predicted), y_test_df))
            if (i == 2) {
              # Calculate the RSq value... weird way was done for first submission...
              SSres <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - predicted_values$s0)^2)
              SStot <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - mean(predicted_values$F1_Exec_Comp_Cog_Accuracy))^2)
              results_df[k, "RSq_2"] <- 1 - SSres/SStot
            } else if (i == 5) {
              # Calculate the RSq value... weird way was done for first submission...
              SSres <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - predicted_values$s0)^2)
              SStot <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - mean(predicted_values$F1_Exec_Comp_Cog_Accuracy))^2)
              results_df[k, "RSq_5"] <- 1 - SSres/SStot
            }
        	}
        }
        # Calculate the RSq value... weird way was done for first submission...
        SSres <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - predicted_values$s0)^2)
        SStot <- sum((predicted_values$F1_Exec_Comp_Cog_Accuracy - mean(predicted_values$F1_Exec_Comp_Cog_Accuracy))^2)
        results_df[k, "RSq_10"] <- 1 - SSres/SStot
        #results_df[k, "RSq_10"] <- cor(predicted_values$s0, predicted_values$F1_Exec_Comp_Cog_Accuracy)^2
        k=k+1
      }
    }
  }
  a=a+1
}


results_df$Modality <- ordered(results_df$Modality, levels=c("Volume", "GMD", "MD",
  "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "All"))

# 1
toPlotVals_1 <- summarySE(data=results_df[,c('Modality', 'Sex', 'Permuted', "RSq_1")],
  groupvars=c('Modality', 'Sex', 'Permuted'), measurevar='RSq_1')
out.plot_1 <- ggplot(results_df, aes(x=RSq_1, group=Permuted, fill=Permuted)) +
    geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Male",], fill="steelblue2") +
    geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Male",], fill="black") +
    geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Female",], fill="violetred1") +
    geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Female",], fill="black") +
    theme_linedraw() + ggtitle("Across ONE Model") +
    facet_grid(Modality ~ Sex) +
    #coord_cartesian(ylim=c(0,500),xlim=c(-.1,.5)) +
    xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
    ylab("") +
    geom_vline(data = toPlotVals_1[toPlotVals_1$Permuted == "Yes" & toPlotVals_1$Sex == "Female", ],
      mapping = aes(xintercept = RSq_1), linetype = "dashed", color="black") +
    geom_vline(data = toPlotVals_1[toPlotVals_1$Permuted == "No" & toPlotVals_1$Sex == "Female", ],
      mapping = aes(xintercept = RSq_1), linetype = "dashed", color="violetred1") +
    geom_vline(data = toPlotVals_1[toPlotVals_1$Permuted == "Yes" & toPlotVals_1$Sex == "Male", ],
      mapping = aes(xintercept = RSq_1), linetype = "dashed", color="black") +
    geom_vline(data = toPlotVals_1[toPlotVals_1$Permuted == "No" & toPlotVals_1$Sex == "Male", ],
      mapping = aes(xintercept = RSq_1), linetype = "dashed", color="steelblue2")


# 2
toPlotVals_2 <- summarySE(data=results_df[,c('Modality', 'Sex', 'Permuted', "RSq_2")],
  groupvars=c('Modality', 'Sex', 'Permuted'), measurevar='RSq_2')
out.plot_2 <- ggplot(results_df, aes(x=RSq_2, group=Permuted, fill=Permuted)) +
      geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Male",], fill="steelblue2") +
      geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Male",], fill="black") +
      geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Female",], fill="violetred1") +
      geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Female",], fill="black") +
      theme_linedraw() + ggtitle("Across TWO Models") +
      facet_grid(Modality ~ Sex) +
      #coord_cartesian(ylim=c(0,500),xlim=c(-.1,.5)) +
      xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
      ylab("") +
      geom_vline(data = toPlotVals_2[toPlotVals_2$Permuted == "Yes" & toPlotVals_2$Sex == "Female", ],
        mapping = aes(xintercept = RSq_2), linetype = "dashed", color="black") +
      geom_vline(data = toPlotVals_2[toPlotVals_2$Permuted == "No" & toPlotVals_2$Sex == "Female", ],
        mapping = aes(xintercept = RSq_2), linetype = "dashed", color="violetred1") +
      geom_vline(data = toPlotVals_2[toPlotVals_2$Permuted == "Yes" & toPlotVals_2$Sex == "Male", ],
        mapping = aes(xintercept = RSq_2), linetype = "dashed", color="black") +
      geom_vline(data = toPlotVals_2[toPlotVals_2$Permuted == "No" & toPlotVals_2$Sex == "Male", ],
        mapping = aes(xintercept = RSq_2), linetype = "dashed", color="steelblue2")

# 5
toPlotVals_5 <- summarySE(data=results_df[,c('Modality', 'Sex', 'Permuted', "RSq_5")],
  groupvars=c('Modality', 'Sex', 'Permuted'), measurevar='RSq_5')
out.plot_5 <- ggplot(results_df, aes(x=RSq_5, group=Permuted, fill=Permuted)) +
      geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Male",], fill="steelblue2") +
      geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Male",], fill="black") +
      geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Female",], fill="violetred1") +
      geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Female",], fill="black") +
      theme_linedraw() + ggtitle("Across FIVE Models") +
      facet_grid(Modality ~ Sex) +
      #coord_cartesian(ylim=c(0,500),xlim=c(-.1,.5)) +
      xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
      ylab("") +
      geom_vline(data = toPlotVals_5[toPlotVals_5$Permuted == "Yes" & toPlotVals_5$Sex == "Female", ],
        mapping = aes(xintercept = RSq_5), linetype = "dashed", color="black") +
      geom_vline(data = toPlotVals_5[toPlotVals_5$Permuted == "No" & toPlotVals_5$Sex == "Female", ],
        mapping = aes(xintercept = RSq_5), linetype = "dashed", color="violetred1") +
      geom_vline(data = toPlotVals_5[toPlotVals_5$Permuted == "Yes" & toPlotVals_5$Sex == "Male", ],
        mapping = aes(xintercept = RSq_5), linetype = "dashed", color="black") +
      geom_vline(data = toPlotVals_5[toPlotVals_5$Permuted == "No" & toPlotVals_5$Sex == "Male", ],
        mapping = aes(xintercept = RSq_5), linetype = "dashed", color="steelblue2")

# 10
toPlotVals_10 <- summarySE(data=results_df[,c('Modality', 'Sex', 'Permuted', "RSq_10")],
  groupvars=c('Modality', 'Sex', 'Permuted'), measurevar='RSq_10')
out.plot_10 <- ggplot(results_df, aes(x=RSq_10, group=Permuted, fill=Permuted)) +
      geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Male",], fill="steelblue2") +
      geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Male",], fill="black") +
      geom_density(data=results_df[results_df$Permuted=='No' & results_df$Sex=="Female",], fill="violetred1") +
      geom_density(data=results_df[results_df$Permuted=='Yes' & results_df$Sex=="Female",], fill="black") +
      theme_linedraw() + ggtitle("Across TEN Models") +
      facet_grid(Modality ~ Sex) +
      #coord_cartesian(ylim=c(0,500),xlim=c(-.1,.5)) +
      xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
      ylab("") +
      geom_vline(data = toPlotVals_10[toPlotVals_10$Permuted == "Yes" & toPlotVals_10$Sex == "Female", ],
        mapping = aes(xintercept = RSq_10), linetype = "dashed", color="black") +
      geom_vline(data = toPlotVals_10[toPlotVals_10$Permuted == "No" & toPlotVals_10$Sex == "Female", ],
        mapping = aes(xintercept = RSq_10), linetype = "dashed", color="violetred1") +
      geom_vline(data = toPlotVals_10[toPlotVals_10$Permuted == "Yes" & toPlotVals_10$Sex == "Male", ],
        mapping = aes(xintercept = RSq_10), linetype = "dashed", color="black") +
      geom_vline(data = toPlotVals_10[toPlotVals_10$Permuted == "No" & toPlotVals_10$Sex == "Male", ],
        mapping = aes(xintercept = RSq_10), linetype = "dashed", color="steelblue2")

# PROBLEMS image
pdf(file="~/Documents/hiLo/plots/exploreR2acrossmods_fixedlowlambda.pdf", height=12, width=8)
out.plot_1
out.plot_2
out.plot_5
out.plot_10
dev.off()
