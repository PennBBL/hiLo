### This script creates a table of Ns for each modality (may incorporate before
### and after statistics, if requested): Sex x Hi-Me-Lo x Modality
###
### Ellyn Butler
### March 20, 2020

library(tidyverse)

# Load data
mods <- c("volume", "gmd", "md", "cbf", "alff", "reho", "nback", "idemo")
for (mod in mods) { assign(paste0(mod, "_df"), read.csv(paste0("~/Documents/hiLo/data/meanLR/", mod, "Data.csv"))) }

# Create all_df

# Create Hi-Me-Lo definition
hilo_df <- volume_df[,c("bblid", "F1_Exec_Comp_Cog_Accuracy")]
hilo_df <- hilo_df[!is.na(hilo_df$F1_Exec_Comp_Cog_Accuracy),]
rownames(hilo_df) <- 1:nrow(hilo_df)

splits <- quantile(hilo_df$F1_Exec_Comp_Cog_Accuracy, c(.33, .66))
hilo_df$CogGroup <- NA
for (i in 1:nrow(hilo_df)) {
	if (hilo_df[i, "F1_Exec_Comp_Cog_Accuracy"] <= splits[[1]]) {
		hilo_df[i, "CogGroup"] <- "Lo"
	} else if (hilo_df[i, "F1_Exec_Comp_Cog_Accuracy"] > splits[[1]] & hilo_df[i, "F1_Exec_Comp_Cog_Accuracy"] <= splits[[2]]) {
		hilo_df[i, "CogGroup"] <- "Me"
	} else {
		hilo_df[i, "CogGroup"] <- "Hi"
	}
}

for (mod in mods) { assign(paste0(mod, "_df"), merge(get(paste0(mod, "_df")), hilo_df)) }

Mods <- c(rep("Volume", 6), rep("GMD", 6), rep("MD", 6), rep("CBF", 6), rep("ALFF", 6), rep("ReHo", 6), rep("NBack", 6), rep("IdEmo", 6))
Cogs <- rep(c("Hi", "Me", "Lo"), 16)
Sexes <- rep(c("Female", "Female", "Female", "Male", "Male", "Male"), 8)
results_df <- tibble(Modality=Mods, Sex=Sexes, Cognition=Cogs, N=rep(NA, 48))

i=1
for (mod in mods) {
  for (sex in 2:1) {
    for (cog in c("Hi", "Me", "Lo")) {
      tmp_df <- get(paste0(mod, "_df"))
      results_df[i, "N"] <- nrow(tmp_df[tmp_df$CogGroup == cog & tmp_df$sex == sex,])
      results_df[i, "Proportion"] <- results_df[i, "N"]/nrow(tmp_df)
      i=i+1
    }
  }
}


#### Is the proportion of each group of subjects (Sex/Cog) significantly different
#### than in the entire neuroimaging sample?

# Load full 1601 cognitive data
cog_df <- read.csv("~/Documents/hiLo/data/cognitive/CNB_Factor_Scores_GO1-GO2-GO3.csv")
cog_df <- cog_df[cog_df$timepoint == 1,]
cog_df <- cog_df[,c("bblid", "F1_Exec_Comp_Cog_Accuracy")]
demo_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
demo_df <- demo_df[,c("bblid", "scanid", "sex", "ageAtCnb1", "ageAtScan1")]
cog_df <- merge(cog_df, demo_df)
# ~/Documents/hiLo/data/n1601_hiLoDataDump_2018-09-20.csv doesn't have full 1598 (3 of 1601 didn't take CNB)
# ~/Documents/hiLo/datacognitive/n1601_cnb_factor_scores_tymoore_20151006.csv # NOT THIS. NOT THE SAME

# Create CogGroup variable
cog_df$CogGroup <- NA
for (i in 1:nrow(cog_df)) {
	if (cog_df[i, "F1_Exec_Comp_Cog_Accuracy"] <= splits[[1]]) {
		cog_df[i, "CogGroup"] <- "Lo"
	} else if (cog_df[i, "F1_Exec_Comp_Cog_Accuracy"] > splits[[1]] & cog_df[i, "F1_Exec_Comp_Cog_Accuracy"] <= splits[[2]]) {
		cog_df[i, "CogGroup"] <- "Me"
	} else {
		cog_df[i, "CogGroup"] <- "Hi"
	}
}

# Compare proportions: none significant
i=1
for (mod in mods) {
  for (sex in 2:1) {
    for (cog in c("Hi", "Me", "Lo")) {
      tmp_df <- get(paste0(mod, "_df"))
      results_df[i, "ProportionFullSample"] <- nrow(cog_df[cog_df$CogGroup == cog & cog_df$sex == sex,])/nrow(cog_df)
      thisprop <- prop.test(x=c(nrow(cog_df[cog_df$CogGroup == cog & cog_df$sex == sex,]), as.numeric(results_df[i, "N"])), n=c(nrow(cog_df), nrow(tmp_df)))
      print(thisprop$p.value)
      if (thisprop$p.value < .001) {
        results_df[i, "Proportion"] <- paste0(results_df[i, "Proportion"], "***")
      } else if (thisprop$p.value < .01) {
        results_df[i, "Proportion"] <- paste0(results_df[i, "Proportion"], "**")
      } else if  (thisprop$p.value < .05) {
        results_df[i, "Proportion"] <- paste0(results_df[i, "Proportion"], "*")
      }
      i=i+1
    }
  }
}

write.csv(results_df, file="~/Documents/hiLo/tables/proportionsMF-HiMeLo.csv", row.names=FALSE)












#
