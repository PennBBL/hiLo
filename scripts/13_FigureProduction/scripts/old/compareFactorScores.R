### This script compares the age-regressed factor score (which was probably done
### by regressing age, age2, and age3 out of the z-scores for each test, and
### then factor analyzing those residuals) to regressing age out of the
### non-age-regressed factor score.
###
### Ellyn Butler
### February 24, 2020

demo.data <- read.csv("~/Documents/n9498/n9498_demographics_go1_20161212.csv")
cog.data <- read.csv("~/Documents/hiLo/data/cognitive/n1601_cnb_factor_scores_tymoore_20151006.csv")
adon.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
adon.data <- adon.data[, c("bblid", "F1_Exec_Comp_Cog_Accuracy")]
colnames(adon.data) <- c("bblid", "F1_Exec_Comp_Cog_Accuracy_Adon")

df <- merge(demo.data, cog.data, by="bblid")
df <- merge(df, adon.data, by="bblid")
df <- df[!is.na(df$ageAtCnb1) & !is.na(df$F1_Exec_Comp_Res_Accuracy) & !is.na(df$F1_Exec_Comp_Cog_Accuracy_Ar),]
rownames(df) <- 1:nrow(df)

df$age <- scale(df$ageAtCnb1)
df$age2 <- scale((df$age)^2)
df$age3 <- scale((df$age)^3)

thismod <- lm(F1_Exec_Comp_Res_Accuracy ~ age + age2 + age3, data=df)

# Apply the function trained on the training data to the training data and the test data
# to regress out age, age2, age3 and the quality metric from brain features
df[,"F1_Exec_Comp_Res_Accuracy_AgeResidualized"] <- thismod$residuals


plot(df$F1_Exec_Comp_Res_Accuracy_AgeResidualized, df$F1_Exec_Comp_Cog_Accuracy_Ar)
plot(df$F1_Exec_Comp_Res_Accuracy_AgeResidualized, df$F1_Exec_Comp_Cog_Accuracy_Adon)
