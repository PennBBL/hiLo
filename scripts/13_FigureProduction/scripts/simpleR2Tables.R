### Filter the summary values and round for Ruben
###
### Ellyn Butler
### March 12, 2020


nullaltmod <- read.csv("~/Documents/hiLo/data/permutationSummary_half_AgeQA_strongNull_OLSNoPenalizeAgeQA.csv")
justbrainmod <- read.csv("~/Documents/hiLo/data/permutationSummary_half_NoReg.csv")

names(nullaltmod)[names(nullaltmod) == "RSq"] <- "mean"
names(justbrainmod)[names(justbrainmod) == "RSq"] <- "mean"
names(justbrainmod)[names(justbrainmod) == "Permuted"] <- "Null"

nullaltmod <- nullaltmod[,c("Modality", "Sex", "Null", "mean", "median")]
justbrainmod <- justbrainmod[,c("Modality", "Sex", "Null", "mean", "median")]

nullaltmod[, c("mean", "median")] <- round(nullaltmod[, c("mean", "median")], digits=4)
justbrainmod[, c("mean", "median")] <- round(justbrainmod[, c("mean", "median")], digits=4)

write.csv(nullaltmod, file="~/Documents/hiLo/data/age_ageandbrain_R2.csv", row.names=FALSE)
write.csv(justbrainmod, file="~/Documents/hiLo/data/perm_brain_R2.csv", row.names=FALSE)
