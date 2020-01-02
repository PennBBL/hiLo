### This script creates Ruben's favorite scatterplot with Hi-Lo by Sex
###
### Ellyn Butler
### November 22, 2019 - November 25, 2019

# What machine are you working on?
galton=FALSE
mymachine=TRUE

## Load Library(s)
if (galton == TRUE) {
	source("/home/butellyn/ButlerPlotFuncs/plotFuncs.R")
} else if (mymachine == TRUE) {
	source("/Users/butellyn/Documents/ButlerPlotFuncs/plotFuncs.R")
}
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

# Now load the data
if (galton == TRUE) {
	vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/noReg/volumeData.csv')
	cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/noReg/cbfData.csv')
	gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/noReg/gmdData.csv')
	reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/noReg/rehoData.csv')
	alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/noReg/alffData.csv')
	tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/noReg/jlfTRData.csv')
} else if (mymachine == TRUE) {
	vol.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/nomeanLR/volumeData.csv')
	cbf.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/nomeanLR/cbfData.csv')
	gmd.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/nomeanLR/gmdData.csv')
	reho.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/nomeanLR/rehoData.csv')
	alff.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/nomeanLR/alffData.csv')
	tr.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/nomeanLR/jlfTRData.csv')
}

###### Load nback
if (galton == TRUE) {
	vol_df <- read.csv("/home/butellyn/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	nback_df <- read.csv('/home/butellyn/hiLo/data/n2416_nback2minus0_20191118.csv')
	nback_quality_df <- read.csv('/home/butellyn/hiLo/data/n2416_nback2minus0_quality_20191118.csv')
} else if (mymachine == TRUE) {
	vol_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	nback_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n2416_nback2minus0_20191118.csv')
	nback_quality_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n2416_nback2minus0_quality_20191118.csv')
}

colnames(nback_df)[colnames(nback_df) == "id0"] <- "bblid"
colnames(nback_df)[colnames(nback_df) == "id1"] <- "scanid"
colnames(nback_quality_df)[colnames(nback_quality_df) == "id0"] <- "bblid"
colnames(nback_quality_df)[colnames(nback_quality_df) == "id1"] <- "scanid"
nback_quality_df$scanid <- as.character(nback_quality_df$scanid)
for (i in 1:nrow(nback_quality_df)) {
	nback_quality_df[i, "scanid"] <- strsplit(nback_quality_df[i, "scanid"], split="x")[[1]][2]
}

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
nback.modal.data <- nback_df

nback.modal.data <- merge(nback.modal.data, nback_quality_df)
# Merge in rest to get exclusions that were imposed for rest... may be appropriate... told Kosha November 19, 2019
nback.modal.data <- merge(nback.modal.data, alff.modal.data)



##### Load idemo
if (galton == TRUE) {
	print("Not on galton yet")
} else if (mymachine == TRUE) {
	vol_df <- read.csv("/Users/butellyn/Documents/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
	idemo_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n1601_idemo_jlf_roivals_20170710.csv')
	idemo_quality_df <- read.csv('/Users/butellyn/Documents/hiLo/data/task/n1601_idemo_FinalQA_092817.csv')

	# Filter out duplicate and unnecessary columns in idemo_df
	idemo_df <- idemo_df[,c("bblid", "scanid", grep("Task", colnames(idemo_df), value=TRUE))]
	idemo_df <- idemo_df[, !(colnames(idemo_df) %in% grep("1$", colnames(idemo_df), value=TRUE))]
}

idemo_df$idemo_jlf_cope1_Task_Brain_Stem <- NULL
idemo.modal.data <- idemo_df
idemo.modal.data <- merge(idemo.modal.data, idemo_quality_df)

# Merge in vol to get t1Exclude and time range exclusions
idemo.modal.data <- merge(idemo.modal.data, vol.modal.data[,c("bblid", "F1_Exec_Comp_Cog_Accuracy", "sex", "ageAtGo1Scan")])

# Apply idemo exclusion
idemo.modal.data <- idemo.modal.data[idemo.modal.data$idemoExclude == 0,]

# Create Adon's hi-lo definitions and merge with every dataframe
# ./hiLo/scripts/01_DataPrep/scripts/prepareNBins.R
hilo_df <- vol.modal.data[,c("bblid", "F1_Exec_Comp_Cog_Accuracy")]
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

# Ellyn: scale (0, 1) all cognitive, brain, age and quality data
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_', 'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_', 'idemo_jlf_cope1_Task_')
data.names <- c('vol', 'gmd', 'tr', 'cbf', 'alff', 'reho', 'nback', 'idemo')
whitematterValues <- c(1, 0, 1, 1, 0, 0, 0, 0)
cerebellumValues <- c(1, 1, 1, 0, 1, 1, 0, 0)
qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "relMeanRMSMotion", "idemoRelMeanRMSMotion")
for (q in 1:8) {
	# Recode sex
	dfName <- paste(data.names[q], ".modal.data", sep='')
	tmp_df <- get(dfName)
	tmp_df[tmp_df$sex == 2, "sex"] <- "Female"
	tmp_df[tmp_df$sex == 1, "sex"] <- "Male"

	# Filter out Me and rename rows
	tmp_df <- merge(tmp_df, hilo_df)
	tmp_df <- tmp_df[tmp_df$CogGroup != "Me",]
	tmp_df$CogGroup <- factor(tmp_df$CogGroup)
	tmp_df$CogGroup <- factor(tmp_df$CogGroup, levels=c("Lo", "Hi"))

	for (side in c("Right", "Left")) {
		if (side == "Right") { notGrepVal <- "_L_"
		} else { notGrepVal <- "_R_" }

		tmp_df_side <- tmp_df[, !(colnames(tmp_df) %in% grep(notGrepVal, colnames(tmp_df), value=TRUE))]
		tmp_df_side$age <- scale(tmp_df_side$ageAtGo1Scan)
		tmp_df_side$age2 <- scale((tmp_df_side$age)^2)
		tmp_df_side$age3 <- scale((tmp_df_side$age)^3)
		tmp_df_side[, qualitymetrics[q]] <- scale(tmp_df_side[, qualitymetrics[q]])
		tmp_df_side[, grep(grepVals[q], colnames(tmp_df_side))] <- scale(tmp_df_side[, grep(grepVals[q], colnames(tmp_df_side))])

		rownames(tmp_df_side) <- 1:nrow(tmp_df_side)

		assign(paste0(dfName, "_", side), tmp_df_side)
	}
}


thistext <- paste0("Figure 1. Coefficients for High Performers for each region.\nRegion ~ High + Age + Age^2 + Age^3 + QA\nAll continuous variables standardized.\n")

# Get coefficients for F1_Exec_Comp_Cog_Accuracy
basgang <- c("Thal", "Put", "Cau", "Pall", "Acc", "INS_WM")
limbic <- c("PHG", "Hipp", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amy", "MCgG", "LIM_WM")
frontorb <- c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "FRO_WM")
frontdors <- c("PrG", "MSFG", "SMC", "MFG", "SFG")
temporal <- c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG", "TMP_WM")
parietal <- c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG", "PAR_WM")
occipital <- c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG", "OCC_WM")
cerebellum <- c("Cer_1-5", "Cer_6-7", "Cer_8-10", "Cer_Ext")
for (q in 1:8) {
	dfName <- paste0(data.names[q], ".modal.data")
	tmp_df <- get(dfName)

	colstouse <- grep(grepVals[q], colnames(tmp_df), value=TRUE)
	colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]
	if (cerebellumValues[q] == 0) {
		colstouse <- colstouse[!(colstouse %in% grep("Cere", colstouse, value=TRUE))]
	}
	if (whitematterValues[q] == 0) {
		colstouse <- colstouse[!(colstouse %in% grep("WM", colstouse, value=TRUE))]
	}
	colstouse <- colstouse[!(colstouse %in% grep("_R_", colstouse, value=TRUE))]
	colstouse <- gsub("_L_", "_", colstouse)

	sum_df <- data.frame(matrix(NA, nrow=length(colstouse), ncol=10)) ####
	colnames(sum_df) <- c("Label", "Abbrev", "Abbrev2", "Lobe", "Importance", "texttypes", "Right", "Left", "Right_SE", "Left_SE")

	for (i in 1:length(colstouse)) {
		sum_df[i, "Label"] <- colstouse[i]
		intname <- strsplit(colstouse[i], split="_")
		if (q != 7 & q != 8) { thisint = 4
		} else if (q == 7) { thisint = 6
		} else { thisint = 5 }
		if (length(intname[[1]]) > thisint) {
			tmpname <- ""
			for (k in thisint:length(intname[[1]])) {
				if (k == thisint) {
					tmpname <- paste0(tmpname, intname[[1]][k])
				} else {
					tmpname <- paste0(tmpname, "_", intname[[1]][k])
				}
			}
			intname <- tmpname
		} else {
			intname <- intname[[1]][thisint]
		}

		if (intname == "Cerebellar_Vermal_Lobules_I.V") { intname <- "Cer_1-5"
		} else if (intname == "Cerebellar_Vermal_Lobules_VI.VII") { intname <- "Cer_6-7"
		} else if (intname == "Cerebellar_Vermal_Lobules_VIII.X") { intname <- "Cer_8-10"
		} else if (intname == "Accumbens_Area") { intname <- "Acc"
		} else if (intname == "Amygdala") { intname <- "Amy"
		} else if (intname == "Caudate") { intname <- "Cau"
		} else if (intname == "Cerebellum_Exterior") { intname <- "Cer_Ext"
		} else if (intname == "Hippocampus") { intname <- "Hipp"
		} else if (intname == "Pallidum") { intname <- "Pall"
		} else if (intname == "Putamen") { intname <- "Put"
		} else if (intname == "Thalamus_Proper") { intname <- "Thal"
		} else if (intname == "Limbic_Lobe_WM") { intname <- "LWM"
		} else if (intname == "Insular_Lobe_WM") { intname <- "IWM"
		} else if (intname == "Frontal_Lobe_WM") { intname <- "FWM"
		} else if (intname == "Parietal_Lobe_WM") { intname <- "PWM"
		} else if (intname == "Occipital_Lobe_WM") { intname <- "OWM"
		} else if (intname == "Temporal_Lobe_WM") { intname <- "TWM"
		}

		sum_df[i, "Abbrev"] <- intname
		if (intname %in% basgang) { sum_df[i, "Lobe"] <- "Basal Ganglia"
		} else if (intname %in% limbic) { sum_df[i, "Lobe"] <- "Limbic"
		} else if (intname %in% frontorb) { sum_df[i, "Lobe"] <- "Frontal"
		} else if (intname %in% frontdors) { sum_df[i, "Lobe"] <- "Frontal"
		} else if (intname %in% temporal) { sum_df[i, "Lobe"] <- "Temporal"
		} else if (intname %in% parietal) { sum_df[i, "Lobe"] <- "Parietal"
		} else if (intname %in% occipital) { sum_df[i, "Lobe"] <- "Occipital"
		} else if (intname %in% cerebellum) { sum_df[i, "Lobe"] <- "Cerebellum"
		}
	}

	for (side in c("Right", "Left")) {
		dfName <- paste0(data.names[q], ".modal.data_", side)
		tmp_df <- get(dfName)
		if (side == "Right") { GrepVal <- "_R_"
		} else { GrepVal <- "_L_" }

		colnames(tmp_df)[colnames(tmp_df) %in% grep(GrepVal, colnames(tmp_df), value=TRUE)] <- gsub(GrepVal, "_", colnames(tmp_df)[colnames(tmp_df) %in% grep(GrepVal, colnames(tmp_df), value=TRUE)])

		for (j in 1:length(colstouse)) {
			func <- paste0(colstouse[j], "~I(CogGroup)+age+age2+age3+", qualitymetrics[q])
			mod <- lm(as.formula(func), data=tmp_df)

			sum_df[j, side] <- summary(mod)$coefficients[2, 1]
			sum_df[j, paste0(side, "_SE")] <- summary(mod)$coefficients[2, 2]
		}
	}

	thisSE <- max(c(sum_df$Right_SE, sum_df$Left_SE))

	PFITregions <- c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG", "ITG", "STG", "Cau", "MOG")

	sum_df$Lobe <- factor(sum_df$Lobe, levels = c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital", "Cerebellum"))
	for (j in 1:nrow(sum_df)) {
		if (sum_df[j, "Abbrev"] %in% PFITregions) {
			sum_df[j, "Importance"] <- "PFIT"
			sum_df[j, "texttypes"] <- "sans" ####
			sum_df[j, "Abbrev2"] <- sum_df[j, "Abbrev"]
		}
	}

	pfitters <- c(abs(sum_df[sum_df$Importance == "PFIT", "Right"]), abs(sum_df[sum_df$Importance == "PFIT", "Left"]))
	pfitters <- pfitters[!is.na(pfitters)]
	topeight <- rev(pfitters[order(pfitters)])[1:8]

	for (j in 1:nrow(sum_df)) {
		if (abs(sum_df[j, "Right"]) > 2*sum_df[j, "Right_SE"] & abs(sum_df[j, "Left"]) > 2*sum_df[j, "Left_SE"] & (!(sum_df[j, "Abbrev"] %in% PFITregions)) & (abs(sum_df[j, "Right"]) > min(topeight)| abs(sum_df[j, "Left"]) > min(topeight))) {
			sum_df[j, "Importance"] <- "Major"
			sum_df[j, "texttypes"] <- "mono" ####
			sum_df[j, "Abbrev2"] <- sum_df[j, "Abbrev"]
		} else if (!(sum_df[j, "Abbrev"] %in% PFITregions)) {
			sum_df[j, "Importance"] <- "None"
			sum_df[j, "texttypes"] <- "" ####
			sum_df[j, "Abbrev2"] <- ""
		}
	}

	#if (data.names[q] %in% c("vol", "gmd", "md", "nback")) {
	#	minscale <- -.4
	#	maxscale <- .4
	#} else if (data.names[q] %in% c("cbf", "alff", "reho")) {
	#	minscale <- -.25
	#	maxscale <- .25
	#}

	right_df <- get(paste0(data.names[q], ".modal.data_Right"))

	if (data.names[q] == "vol") {
		thistit <- "Volume: Coefficient for High Performers"
		thistext <- paste0(thistext, "Volume: N=", nrow(right_df), "; ")
	} else if (data.names[q] == "gmd") {
		thistit <- "Gray Matter Density: Coefficient for High Performers"
		thistext <- paste0(thistext, "Gray Matter Density: N=", nrow(right_df), "\n")
	} else if (data.names[q] == "tr") {
		thistit <- "Mean Diffusivity: Coefficient for High Performers"
		thistext <- paste0(thistext, "Mean Diffusivity: N=", nrow(right_df), "; ")
	} else if (data.names[q] == "cbf") {
		thistit <- "Cerebral Blood Flow: Coefficient for High Performers"
		thistext <- paste0(thistext, "Cerebral Blood Flow: N=", nrow(right_df), "\n")
	} else if (data.names[q] == "alff") {
		thistit <- "Amp of Low Freq Fluc: Coefficient for High Performers"
		thistext <- paste0(thistext, "Amp of Low Freq Fluc: N=", nrow(right_df), "; ")
	} else if (data.names[q] == "reho") {
		thistit <- "Regional Homogeneity: Coefficient for High Performers"
		thistext <- paste0(thistext, "Regional Homogeneity: N=", nrow(right_df), "\n")
	} else if (data.names[q] == "nback") {
		thistit <- "2 - 0 Back: Coefficient for High Performers"
		thistext <- paste0(thistext, "2 - 0 Back: N=", nrow(right_df), "; ")
	} else if (data.names[q] == "idemo") {
		thistit <- "IDEMO Task: Coefficient for High Performers"
		thistext <- paste0(thistext, "IDEMO Task: N=", nrow(right_df))
	}

	sum_df$Importance <- ordered(sum_df$Importance, levels=c("None", "Major", "PFIT"))

	lowerbound <- force(-thisSE*2)
	upperbound <- force(thisSE*2)

	minscale <- round(min(sum_df$Right, sum_df$Left), digits=1) ####
	maxscale <- round(max(sum_df$Right, sum_df$Left), digits=1) ####
	if (minscale > min(c(sum_df$Right, sum_df$Left, lowerbound))) {
		minscale <- round(min(c(sum_df$Right, sum_df$Left, lowerbound)), digits=1) - .1
	}
	if (maxscale < max(c(sum_df$Right, sum_df$Left, upperbound))) {
		maxscale <- round(max(c(sum_df$Right, sum_df$Left, upperbound)), digits=1) + .1
	}

	# November 22, 2019: Try giving more space for volume
	if (data.names[q] == "vol") {
		maxscale <- maxscale + .1
	}

	levelvec <- as.character(unique(sum_df$Importance))

	if (identical(levelvec[order(levelvec)], c("Major", "None", "PFIT"))) { ####
		thesecolors <- c("grey60", "grey30", "black") ####
		theseshapes <- c(16, 17, 15) ####
	} else if (identical(levelvec[order(levelvec)], c("None", "PFIT"))) { ####
		thesecolors <- c("grey60", "black") ####
		theseshapes <- c(16, 15) ####
	} else { print(q) }####

	plot_fun <- function(sum_df, lowerbound, upperbound, thesecolors) {
		force(lowerbound)
		force(upperbound)
		ggplot(sum_df, aes(x=Left, y=Right)) + theme_minimal() +
		geom_rect(aes(xmin=lowerbound, xmax=upperbound, ymin=-Inf, ymax=Inf), fill="grey97", color=NA, alpha=.2, size=0) +
		geom_rect(aes(ymin=lowerbound, ymax=upperbound, xmin=-Inf, xmax=Inf), fill="grey97", color=NA, alpha=.2, size=0) +
		geom_point(aes(color=Importance, shape=Importance), size=2) +  ####
		geom_text_repel(aes(label=Abbrev2, color=Importance, family=texttypes), show.legend=FALSE, size=6, nudge_x=c(minscale/5, maxscale/5), nudge_y=c(minscale/8, maxscale/8)) + ####
		guides(text_repel=FALSE) + scale_shape_manual(values=theseshapes) + ####
		scale_color_manual(values=thesecolors) + labs(title=thistit) +
		xlab("Left Effect Size (95% CI)") + ylab("Right Effect Size (95% CI)") +
		geom_abline(intercept=0, slope=1, linetype="dotdash", alpha=.5) + geom_smooth(method="lm", formula=y~x, se=FALSE, color="red") + #might remove
		geom_hline(yintercept=0, linetype="dashed", color="blue") + geom_vline(xintercept=0, linetype="dashed", color="blue") +
		geom_hline(yintercept=thisSE*2, linetype="dashed", color="slateblue") + geom_vline(xintercept=thisSE*2, linetype="dashed", color="slateblue") +
		geom_hline(yintercept=-thisSE*2, linetype="dashed", color="slateblue") + geom_vline(xintercept=-thisSE*2, linetype="dashed", color="slateblue") +
		annotate(geom="text", x=minscale+(maxscale-minscale)/8, y=maxscale, size=6, label=paste0("r = ", round(cor(sum_df$Right, sum_df$Left), digits=3))) +
		theme(plot.title = element_text(size=15, face="bold"), axis.title = element_text(size=15), axis.text = element_text(size=15), axis.text.x = element_text(angle = 90))+
		scale_x_continuous(limits=c(minscale, maxscale), breaks=round(seq(minscale, maxscale, .1), digits=2)) + ####
		scale_y_continuous(limits=c(minscale, maxscale), breaks=round(seq(minscale, maxscale, .1), digits=2))  ####
	}

	outPlot <- plot_fun(sum_df, lowerbound, upperbound, thesecolors)

	assign(paste0(data.names[q], "_plot"), outPlot)
}

obj <- grid.text(thistext)

if (galton == TRUE) {
	pdf(paste0('/home/butellyn/hiLo/plots/beta_HiLo_RL_', Sys.Date(), '.pdf'), width=20, height=18)
	grid.arrange(vol_plot, gmd_plot, tr_plot, nback_plot, obj, idemo_plot, cbf_plot, alff_plot, reho_plot, nrow=3, ncol=3)
	dev.off()
} else if (mymachine == TRUE) {
	pdf(paste0('/Users/butellyn/Documents/hiLo/plots/beta_HiLo_RL_', Sys.Date(), '.pdf'), width=20, height=18)
	grid.arrange(vol_plot, gmd_plot, tr_plot, nback_plot, obj, idemo_plot, cbf_plot, alff_plot, reho_plot, nrow=3, ncol=3)
	dev.off()
}
