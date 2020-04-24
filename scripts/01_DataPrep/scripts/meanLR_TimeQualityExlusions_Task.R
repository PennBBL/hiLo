### This script applies time and quality exclusions to nback and idemo data,
### and averages the left and right hemispheres, weighting by volume
###
### Ellyn Butler
### April 1, 2020 - April 3, 2020

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
	###nback_quality_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/nback2416QA_2018-10-21.csv')
  nback_quality_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n1601_nback_qa_20200319.csv')
  nback_quality_df <- nback_quality_df[,c("bblid", "scanid", "nbackrelMeanRMSMotion2",
                        "nbackrelMaxRMSMotion2", "nbackZerobackNrExclude",
                        "nbackIncompleteBehExclude", "nbackExclude2")]
  names(nback_quality_df) <- c("bblid", "scanid", "nbackRelMeanRMSMotion",
                        "nbackRelMaxRMSMotion", "nbackZerobackNrExclude",
                        "nbackIncompleteBehExclude", "nbackExclude")
}

names(nback_df)[names(nback_df) == "id0"] <- "bblid"
names(nback_df)[names(nback_df) == "id1"] <- "scanid"
#nback_quality_df <- nback_quality_df[nback_quality_df$scanid %in% vol_df$scanid, ]

nback_df <- nback_df[nback_df$scanid %in% vol_df$scanid, ]
vol_df <- vol_df[vol_df$scanid %in% nback_df$scanid, ]
vol_df <- vol_df[,c("bblid", grep("mprage_jlf_vol", names(vol_df), value=TRUE))]
vol_df <- vol_df[,!(names(vol_df) %in% grep("WM", names(vol_df), value=TRUE))]
vol_df <- vol_df[,!(names(vol_df) %in% paste0("mprage_jlf_vol_", c("ICV", "TBV", "TBGM")))]
nback_df <- arrange(nback_df, bblid)
vol_df <- arrange(vol_df, bblid)

sigchange_vec <- grep("sigchange", names(nback_df), value=TRUE)
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
	volnames <- c(volnames, grep(sigc, names(vol_df), value=TRUE))
}
volnames <- unique(volnames)
vol_df <- vol_df[,c("bblid", volnames)]
vol_df <- vol_df[order(vol_df$bblid),]
row.names(vol_df) <- 1:nrow(vol_df)
nback_df <- nback_df[order(nback_df$bblid),]
row.names(nback_df) <- 1:nrow(nback_df)
nback_df$scanid <- NULL
nback.modal.data <- averageLeftAndRight_WeightByVol(vol_df, nback_df, volString="mprage_jlf_vol", otherString="sigchange_contrast4_2back0back_mean_miccai", rightString="_R_", leftString="_L_", averageString="_ave_")
nback.modal.data <- nback.modal.data[,c("bblid", grep("ave", names(nback.modal.data), value=TRUE))]

nback.modal.data <- merge(nback.modal.data, nback_quality_df)

other_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
other_df <- other_df[,c("bblid", "scanid", "F1_Exec_Comp_Cog_Accuracy", "sex", "ageAtScan1", "ageAtCnb1")]
names(other_df)[names(other_df) == "ageAtScan1"] <- "ageAtGo1Scan"

other_df <- other_df[other_df$scanid %in% nback.modal.data$scanid,]
nback.modal.data <- merge(nback.modal.data, other_df)

# Apply time and quality exclusions
#nback.modal.data <- nback.modal.data[nback.modal.data$nbackFcExclude == 0 & nback.modal.data$nbackFcExcludeVoxelwise == 0 & nback.modal.data$nbackNoDataExclude == 0 & nback.modal.data$nbackRelMeanRMSMotionExclude == 0 & nback.modal.data$nbackNSpikesMotionExclude == 0 & nback.modal.data$nbackVoxelwiseCoverageExclude == 0 ,]
nback.modal.data <- nback.modal.data[nback.modal.data$nbackExclude == 0,]
nback.modal.data$absagediff <- abs(nback.modal.data$ageAtCnb1 - nback.modal.data$ageAtGo1Scan)
nback.modal.data <- nback.modal.data[nback.modal.data$absagediff <= 12,]
nback.modal.data <- nback.modal.data[!is.na(nback.modal.data$ageAtCnb1),]
row.names(nback.modal.data) <- 1:nrow(nback.modal.data)


##### Average L & R for idemo (weight by vol)
if (galton == TRUE) {
	print("Not on galton yet")
} else if (mymachine == TRUE) {
	vol_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	idemo_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n2416_idemotask_20191219.csv')
	idemo_quality_df <- read.csv('~/Documents/hiLo/data/task/n1601_idemo_FinalQA_092817.csv')
  idemo_quality_df2 <- read.csv('~/Documents/hiLo/data/task/n1601_go1_idemo_qa_behav_vars.csv')
  idemo_quality_df <- merge(idemo_quality_df, idemo_quality_df2)

	names(idemo_df)[names(idemo_df) == "id0"] <- "bblid"
	names(idemo_df)[names(idemo_df) == "id1"] <- "scanid"
}

vol_df <- vol_df[,c("bblid", "scanid", grep("mprage_jlf_vol", names(vol_df), value=TRUE))] ######
vol_df <- vol_df[,!(names(vol_df) %in% grep("WM", names(vol_df), value=TRUE))]
vol_df <- vol_df[,!(names(vol_df) %in% paste0("mprage_jlf_vol_", c("ICV", "TBV", "TBGM")))]

sigchange_vec <- grep("sigchange", names(idemo_df), value=TRUE)
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
	volnames <- c(volnames, grep(sigc, names(vol_df), value=TRUE))
}
volnames <- unique(volnames)
vol_df <- vol_df[,c("bblid", "scanid", volnames)]
vol_df <- vol_df[order(vol_df$bblid),]
row.names(vol_df) <- 1:nrow(vol_df)
idemo_df <- idemo_df[order(idemo_df$bblid),]
row.names(idemo_df) <- 1:nrow(idemo_df)
idemo_df <- idemo_df[idemo_df$scanid %in% vol_df$scanid,]
row.names(idemo_df) <- 1:nrow(idemo_df)

vol_df <- vol_df[vol_df$scanid %in% idemo_df$scanid,]
row.names(vol_df) <- 1:nrow(vol_df)

idemo.modal.data <- averageLeftAndRight_WeightByVol(vol_df, idemo_df, volString="mprage_jlf_vol", otherString="sigchange_cope1_task_mean_miccai", rightString="_R_", leftString="_L_", averageString="_ave_")
idemo.modal.data <- idemo.modal.data[,c("bblid", grep("ave", names(idemo.modal.data), value=TRUE))]

idemo.modal.data <- merge(idemo.modal.data, idemo_quality_df)

# Apply time range exclusions (applied if using volumeData.csv)
other_df <- read.csv("~/Documents/hiLo/data/meanLR/volumeData.csv")
other_df <- other_df[,c("bblid", "scanid", "F1_Exec_Comp_Cog_Accuracy", "sex", "ageAtGo1Scan")]

#other_df <- other_df[other_df$scanid %in% idemo.modal.data$scanid,]
idemo.modal.data <- merge(idemo.modal.data, other_df)

# Apply idemoExclude and behavioral exclude (non responses >= 50% (30 and more))
idemo.modal.data <- idemo.modal.data[idemo.modal.data$idemoExclude == 0 & idemo.modal.data$all_all_nr_count_idemo < 30,]
idemo.modal.data <- idemo.modal.data[!is.na(idemo.modal.data$sigchange_cope1_task_mean_miccai_ave_Accumbens_Area),]

row.names(idemo.modal.data) <- 1:nrow(idemo.modal.data)



write.csv(nback.modal.data, file="~/Documents/hiLo/data/meanLR/nbackData.csv", row.names=FALSE)
write.csv(idemo.modal.data, file="~/Documents/hiLo/data/meanLR/idemoData.csv", row.names=FALSE)
