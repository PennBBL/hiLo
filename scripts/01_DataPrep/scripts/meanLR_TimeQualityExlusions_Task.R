### This script applies time and quality exclusions to nback and idemo data,
### and averages the left and right hemispheres, weighting by volume
###
### Ellyn Butler
### February 10, 2020

# What machine are you working on?
galton=FALSE
mymachine=TRUE

if (galton == TRUE) {
  source("/home/butellyn/ButlerPlotFuncs/plotFuncs.R")
  stop("Facet nested isn't here yet")
	vol_df <- read.csv("/home/butellyn/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	nback_df <- read.csv('/home/butellyn/hiLo/data/n2416_nback2minus0_20191118.csv')
	stop("Correct nback quality not here")
} else if (mymachine == TRUE) {
  source("~/Documents/ButlerPlotFuncs/plotFuncs.R")
  source("~/Documents/ButlerPlotFuncs/facetnested.R")
	vol_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	nback_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n2416_nback2minus0_20191118.csv')
	nback_quality_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/nback2416QA_2018-10-21.csv')
}

colnames(nback_df)[colnames(nback_df) == "id0"] <- "bblid"
colnames(nback_df)[colnames(nback_df) == "id1"] <- "scanid"
nback_quality_df <- nback_quality_df[nback_quality_df$scanid %in% vol_df$scanid, ]

nback_df <- nback_df[nback_df$scanid %in% vol_df$scanid, ]
vol_df <- vol_df[vol_df$scanid %in% nback_df$scanid, ]
vol_df <- vol_df[,c("bblid", grep("mprage_jlf_vol", colnames(vol_df), value=TRUE))]
vol_df <- vol_df[,!(colnames(vol_df) %in% grep("WM", colnames(vol_df), value=TRUE))]
vol_df <- vol_df[,!(colnames(vol_df) %in% paste0("mprage_jlf_vol_", c("ICV", "TBV", "TBGM")))]

sigchange_vec <- grep("sigchange", colnames(nback_df), value=TRUE)
signames <- c()
for (sigc in sigchange_vec) {
	tmp <- strsplit(sigc, split="_")
	tmp2 <- ""
	for (i in 7:length(tmp[[1]])) {
		if (i > 7) {
			tmp2 <- paste0(tmp2, "_", tmp[[1]][i])
		} else {
			tmp2 <- tmp[[1]][i]
		}
	}
	signames <- c(signames, tmp2)
}
signames <- unique(signames)
volnames <- c()
for (sigc in signames) {
	volnames <- c(volnames, grep(sigc, colnames(vol_df), value=TRUE))
}
volnames <- unique(volnames)
vol_df <- vol_df[,c("bblid", volnames)]
vol_df <- vol_df[order(vol_df$bblid),]
rownames(vol_df) <- 1:nrow(vol_df)
nback_df <- nback_df[order(nback_df$bblid),]
rownames(nback_df) <- 1:nrow(nback_df)
nback_df$scanid <- NULL
nback.modal.data <- averageLeftAndRight_WeightByVol(vol_df, nback_df, volString="mprage_jlf_vol", otherString="sigchange_contrast4_2back0back_mean_miccai", rightString="_R_", leftString="_L_", averageString="_ave_")
nback.modal.data <- nback.modal.data[,c("bblid", grep("ave", colnames(nback.modal.data), value=TRUE))]

nback.modal.data <- merge(nback.modal.data, nback_quality_df)

other_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
other_df <- other_df[,c("bblid", "scanid", "F1_Exec_Comp_Cog_Accuracy", "sex", "ageAtScan1", "ageAtCnb1")]
names(other_df)[names(other_df) == "ageAtScan1"] <- "ageAtGo1Scan"

other_df <- other_df[other_df$scanid %in% nback.modal.data$scanid,]
nback.modal.data <- merge(nback.modal.data, other_df)

# Apply time and quality exclusions
nback.modal.data <- nback.modal.data[nback.modal.data$nbackFcExclude == 0 & nback.modal.data$nbackFcExcludeVoxelwise == 0 & nback.modal.data$nbackNoDataExclude == 0 & nback.modal.data$nbackRelMeanRMSMotionExclude == 0 & nback.modal.data$nbackNSpikesMotionExclude == 0 & nback.modal.data$nbackVoxelwiseCoverageExclude == 0 ,]
nback.modal.data$absagediff <- abs(nback.modal.data$ageAtCnb1 - nback.modal.data$ageAtGo1Scan)
nback.modal.data <- nback.modal.data[nback.modal.data$absagediff <= 12,]
nback.modal.data <- nback.modal.data[!is.na(nback.modal.data$ageAtCnb1),]
rownames(nback.modal.data) <- 1:nrow(nback.modal.data)


##### Average L & R for idemo (weight by vol)
if (galton == TRUE) {
	print("Not on galton yet")
} else if (mymachine == TRUE) {
	vol_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	idemo_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n2416_idemotask_20191219.csv')
	idemo_quality_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n1601_idemo_FinalQA_092817.csv')

	names(idemo_df)[names(idemo_df) == "id0"] <- "bblid"
	names(idemo_df)[names(idemo_df) == "id1"] <- "scanid"

	# Filter out duplicate and unnecessary columns in idemo_df
	#idemo_df <- idemo_df[,c("bblid", "scanid", grep("Task", colnames(idemo_df), value=TRUE))]
	#idemo_df <- idemo_df[, !(colnames(idemo_df) %in% grep("1$", colnames(idemo_df), value=TRUE))]
}

vol_df <- vol_df[,c("bblid", "scanid", grep("mprage_jlf_vol", colnames(vol_df), value=TRUE))]
vol_df <- vol_df[,!(colnames(vol_df) %in% grep("WM", colnames(vol_df), value=TRUE))]
vol_df <- vol_df[,!(colnames(vol_df) %in% paste0("mprage_jlf_vol_", c("ICV", "TBV", "TBGM")))]

#idemo_df$idemo_jlf_cope1_Task_Brain_Stem <- NULL
sigchange_vec <- grep("sigchange", colnames(idemo_df), value=TRUE)
signames <- c()
for (sigc in sigchange_vec) {
	tmp <- strsplit(sigc, split="_")
	tmp2 <- ""
	for (i in 7:length(tmp[[1]])) {
		if (i > 7) {
			tmp2 <- paste0(tmp2, "_", tmp[[1]][i])
		} else {
			tmp2 <- tmp[[1]][i]
		}
	}
	signames <- c(signames, tmp2)
}
signames <- unique(signames)
volnames <- c()
for (sigc in signames) {
	volnames <- c(volnames, grep(sigc, colnames(vol_df), value=TRUE))
}
volnames <- unique(volnames)
vol_df <- vol_df[,c("bblid", "scanid", volnames)]
vol_df <- vol_df[order(vol_df$bblid),]
rownames(vol_df) <- 1:nrow(vol_df)
idemo_df <- idemo_df[order(idemo_df$bblid),]
rownames(idemo_df) <- 1:nrow(idemo_df)
idemo_df <- idemo_df[idemo_df$scanid %in% vol_df$scanid,]

vol_df <- vol_df[vol_df$scanid %in% idemo_df$scanid,]
rownames(vol_df) <- 1:nrow(vol_df)

idemo.modal.data <- averageLeftAndRight_WeightByVol(vol_df, idemo_df, volString="mprage_jlf_vol", otherString="sigchange_cope1_task_mean_miccai", rightString="_R_", leftString="_L_", averageString="_ave_")
idemo.modal.data <- idemo.modal.data[,c("bblid", grep("ave", colnames(idemo.modal.data), value=TRUE))]

idemo.modal.data <- merge(idemo.modal.data, idemo_quality_df)

# Apply time range exclusions
other_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
other_df <- other_df[,c("bblid", "scanid", "F1_Exec_Comp_Cog_Accuracy", "sex", "ageAtScan1", "ageAtCnb1")]
names(other_df)[names(other_df) == "ageAtScan1"] <- "ageAtGo1Scan"

other_df <- other_df[other_df$scanid %in% idemo.modal.data$scanid,]
idemo.modal.data <- merge(idemo.modal.data, other_df)
idemo.modal.data <- idemo.modal.data[idemo.modal.data$idemoExclude == 0,]

idemo.modal.data$absagediff <- abs(idemo.modal.data$ageAtCnb1 - idemo.modal.data$ageAtGo1Scan)
idemo.modal.data <- idemo.modal.data[idemo.modal.data$absagediff <= 12,]
idemo.modal.data <- idemo.modal.data[!is.na(idemo.modal.data$ageAtCnb1),]
rownames(idemo.modal.data) <- 1:nrow(idemo.modal.data)



write.csv(nback.modal.data, file="~/Documents/hiLo/data/meanLR/nbackData.csv", row.names=FALSE)
write.csv(idemo.modal.data, file="~/Documents/hiLo/data/meanLR/idemoData.csv", row.names=FALSE)
