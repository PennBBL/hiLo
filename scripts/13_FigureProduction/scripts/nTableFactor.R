### This script creates a table of Ns for each modality (may incorporate before
### and after statistics, if requested): Sex x Modality
###
### Ellyn Butler
### March 20, 2020 - April 13, 2020

library(tidyverse)

# Load data
mods <- c("volume", "gmd", "md", "cbf", "alff", "reho", "nback", "idemo")
for (mod in mods) { assign(paste0(mod, "_df"), read.csv(paste0("~/Documents/hiLo/data/meanLR/", mod, "Data.csv"))) }

#for (mod in mods) { assign(paste0(mod, "_df"), merge(get(paste0(mod, "_df")), cog_df)) }

Mods <- c(rep("Volume", 4), rep("GMD", 4), rep("MD", 4), rep("CBF", 4), rep("ALFF", 4), rep("ReHo", 4), rep("NBack", 4), rep("IdEmo", 4))
Sexes <- rep(c("Female", "Female", "Male", "Male"), 8)
Measures <- rep(c("Cognition", "Age"), 16)
results_df <- data.frame(Modality=Mods, Sex=Sexes, Measure=Measures,
	NBefore=rep(0, length(Sexes)), NAfter=rep(0, length(Sexes)),
	MeanBefore=rep(0, length(Sexes)), MeanAfter=rep(0, length(Sexes)),
	SDBefore=rep(0, length(Sexes)), SDAfter=rep(0, length(Sexes)),
	PValue=rep(0, length(Sexes)))


# Load full 1601 cognitive data
cog_df <- read.csv("~/Documents/hiLo/data/cognitive/CNB_Factor_Scores_GO1-GO2-GO3.csv") ### AGE-REG
cog_df <- cog_df[,c("bblid", "timepoint", "bblid_timepoint", "F1_Exec_Comp_Cog_Accuracy")]
demo_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
demo_df <- demo_df[,c("bblid", "timepoint", "bblid_timepoint", "scanid", "sex", "ageAtCnb1", "ageAtScan1")]
cog_df <- merge(cog_df, demo_df)


#### Compare mean performance and age before and after exclusions
#### for males and females
i=1
for (mod in mods) {
  for (sex in 2:1) {
	    tmp_df <- get(paste0(mod, "_df"))
			results_df[i, "NBefore"] <- nrow(cog_df[cog_df$sex == sex,])
			results_df[i, "NAfter"] <- nrow(tmp_df[tmp_df$sex == sex,])
			results_df[i+1, "NBefore"] <- NA
			results_df[i+1, "NAfter"] <- NA

			### Cognition
			cogGroup_df <- data.frame(cognition=c(cog_df[cog_df$sex == sex, "F1_Exec_Comp_Cog_Accuracy"],
				tmp_df[tmp_df$sex == sex, "F1_Exec_Comp_Cog_Accuracy"]),
				group=c(rep("Full", nrow(cog_df[cog_df$sex == sex, ])),
				rep("Limited", nrow(tmp_df[tmp_df$sex == sex, ]))))
			results_df[i, "MeanBefore"] <- round(mean(cog_df[cog_df$sex == sex, "F1_Exec_Comp_Cog_Accuracy"])/12, digits=4)
			results_df[i, "MeanAfter"] <- round(mean(tmp_df[tmp_df$sex == sex, "F1_Exec_Comp_Cog_Accuracy"])/12, digits=4)
			results_df[i, "SDBefore"] <- round(sd(cog_df[cog_df$sex == sex, "F1_Exec_Comp_Cog_Accuracy"])/12, digits=4)
			results_df[i, "SDAfter"] <- round(sd(tmp_df[tmp_df$sex == sex, "F1_Exec_Comp_Cog_Accuracy"])/12, digits=4)
			results_df[i, "PValue"] <- round(t.test(cognition ~ group, data=cogGroup_df)$p.value, digits=4)

			### Age
			ageGroup_df <- data.frame(age=c(cog_df[cog_df$sex == sex, "ageAtScan1"],
				tmp_df[tmp_df$sex == sex, "ageAtGo1Scan"]),
				group=c(rep("Full", nrow(cog_df[cog_df$sex == sex, ])),
				rep("Limited", nrow(tmp_df[tmp_df$sex == sex, ]))))
			results_df[i+1, "MeanBefore"] <- round(mean(cog_df[cog_df$sex == sex, "ageAtScan1"])/12, digits=1)
			results_df[i+1, "MeanAfter"] <- round(mean(tmp_df[tmp_df$sex == sex, "ageAtGo1Scan"])/12, digits=1)
			results_df[i+1, "SDBefore"] <- round(sd(cog_df[cog_df$sex == sex, "ageAtScan1"])/12, digits=1)
			results_df[i+1, "SDAfter"] <- round(sd(tmp_df[tmp_df$sex == sex, "ageAtGo1Scan"])/12, digits=1)
			results_df[i+1, "PValue"] <- round(t.test(age ~ group, data=ageGroup_df)$p.value, digits=4)

      i=i+2
  }
}


write.csv(results_df, file="~/Documents/hiLo/tables/nTable_SexCogAge.csv", row.names=FALSE)












#
