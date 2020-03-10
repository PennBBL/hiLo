### This script simulates regressing age out of the DV and IVs
### and calculates R2 in the test set
###
### Ellyn Butler
### March 5, 2020

set.seed(20)

library('mvtnorm')
library('glmnet')
library('caret')
library('gridExtra')

# Create dataframes
df <- data.frame(matrix(NA, nrow=2000, ncol=6))
colnames(df) <- c("ID", "Cog", "Age", "Brain1", "Brain2", "Brain3")
df$ID <- paste0("sub", 1:2000)

# Simulate data from multivariate normal
Sigma <- matrix(c(1, .6, .2, .3, .29, .6, 1, .3, .4, .3, .2, .3, 1, .8, .9, .3,
  .4, .8, 1, .85, .29, .3, .9, .85, 1), nrow=5, ncol=5)
df[, 2:6] <- rmvnorm(2000, mean=c(0, 12, 5, 4, 5.5), Sigma)

results_df <- data.frame(matrix(NA, nrow=1000, ncol=5))
colnames(results_df) <- c("Iteration", "Using_Age", "Using_AgeBrain", "Using_Brain", "Using_BrainRegAge")
results_df$Iteration <- 1:nrow(results_df)

for (i in 1:1000) {
  folds <- createFolds(df$Cog, k=2, list=TRUE)
  train_df <- df[folds[[1]],]
  test_df <- df[folds[[2]],]
  rownames(train_df) <- 1:nrow(train_df)
  rownames(test_df) <- 1:nrow(test_df)

  ##### Age
  thismod <- lm(Cog ~ Age, data=train_df)
  train_df$Cog_predicted_age <- predict(thismod)
  test_df$Cog_predicted_age <- predict(thismod, newdata=test_df)
  RSS <- sum((test_df$Cog - test_df$Cog_predicted_age)^2)
  TSS <- sum((test_df$Cog - mean(train_df$Cog))^2)
  results_df[i, "Using_Age"] <- 1 - RSS/TSS

  ##### Age and brain
  train_input <- as.matrix(train_df[, c("Age", paste0("Brain", 1:3))])
  Cog <- train_df$Cog

  # Train the ridge model
  ridge_cv <- cv.glmnet(train_input, Cog, nfolds=5, exclude=c(1))
  lambda_cv <- ridge_cv$lambda.min
  ridge_model <- glmnet(train_input, Cog, lambda=lambda_cv)
  test_df$Cog_predicted_agebrain <- predict(ridge_model, newx=as.matrix(test_df[, c("Age", paste0("Brain", 1:3))]))

  RSS <- sum((test_df$Cog - test_df$Cog_predicted_agebrain)^2)
  TSS <- sum((test_df$Cog - mean(train_df$Cog))^2)
  results_df[i, "Using_AgeBrain"] <- 1 - RSS/TSS

  ##### Without regressing out age
  train_input <- as.matrix(train_df[, c(paste0("Brain", 1:3))])
  Cog <- train_df$Cog

  # Train the ridge model
  ridge_cv <- cv.glmnet(train_input, Cog, nfolds=5)
  lambda_cv <- ridge_cv$lambda.min
  ridge_model <- glmnet(train_input, Cog, lambda=lambda_cv)
  test_df$Cog_predicted_noregress <- predict(ridge_model, newx=as.matrix(test_df[, c(paste0("Brain", 1:3))]))

  RSS <- sum((test_df$Cog - test_df$Cog_predicted_noregress)^2)
  TSS <- sum((test_df$Cog - mean(train_df$Cog))^2)
  results_df[i, "Using_Brain"] <- 1 - RSS/TSS #.0609


  ##### Regress age out of DV and IVs
  for (thiscol in c("Cog", "Brain1", "Brain2", "Brain3")) {
    thisfunc <- paste0(thiscol, " ~ Age")
    thismod <- lm(formula(thisfunc), data=train_df)
    train_df[,paste0(thiscol, "_agereg")] <- thismod$residuals
    test_df[,paste0(thiscol, "_agereg")] <- test_df[, thiscol] - predict(thismod, newdata=test_df)
  }

  train_input <- as.matrix(train_df[, c(paste0("Brain", 1:3, "_agereg"))])
  Cog_agereg <- train_df$Cog_agereg

  # Train the ridge model
  ridge_cv <- cv.glmnet(train_input, Cog_agereg, nfolds=5)
  lambda_cv <- ridge_cv$lambda.min
  ridge_model <- glmnet(train_input, Cog_agereg, lambda=lambda_cv)
  test_df$Cog_predicted_regress <- predict(ridge_model, newx=as.matrix(test_df[, c(paste0("Brain", 1:3, "_agereg"))]))

  RSS <- sum((test_df$Cog_agereg - test_df$Cog_predicted_regress)^2)
  TSS <- sum((test_df$Cog_agereg - mean(train_df$Cog_agereg))^2)
  results_df[i, "Using_BrainRegAge"] <- 1 - RSS/TSS # .0533...
  #Different, but probably isn't as different as it should be if all of the variance
  #explained by age were partialed out
}

results_df$Diff_Age_AgeBrain <- results_df$Using_AgeBrain - results_df$Using_Age

age_plot <- ggplot(results_df, aes(Using_Age)) + theme_linedraw() +
    geom_histogram() + coord_cartesian(xlim=c(-.2, .5)) +
    ggtitle("Age")

agebrain_plot <- ggplot(results_df, aes(Using_AgeBrain)) + theme_linedraw() +
    geom_histogram() + coord_cartesian(xlim=c(-.2, .5)) +
    ggtitle("Age and Brain")

brain_plot <- ggplot(results_df, aes(Using_Brain)) + theme_linedraw() +
    geom_histogram() + coord_cartesian(xlim=c(-.2, .5)) +
    ggtitle("Brain")

diff_plot <- ggplot(results_df, aes(Diff_Age_AgeBrain)) + theme_linedraw() +
    geom_histogram() + coord_cartesian(xlim=c(-.2, .5)) +
    ggtitle("Diff: Age vs. Age and Brain")

agereg_plot <- ggplot(results_df, aes(Using_BrainRegAge)) + theme_linedraw() +
    geom_histogram() + coord_cartesian(xlim=c(-.2, .5)) +
    ggtitle("Age Regressed Cog and Brain")

pdf(file="~/Documents/hiLo/plots/sim_regageVdiff.pdf", width=12, height=8)
grid.arrange(age_plot, agebrain_plot, brain_plot, diff_plot, agereg_plot, ncol=3, nrow=2)
dev.off()
