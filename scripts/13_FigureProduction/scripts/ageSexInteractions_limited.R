### This script plots age and sex interactions for all modalities, limiting
### the number of regions by effect size
###
### Ellyn Butler
### December 16, 2019

# What machine are you working on?
galton=FALSE
mymachine=TRUE

## Load Library(s)
if (galton == TRUE) {
	source("/home/butellyn/ButlerPlotFuncs/plotFuncs.R")
  print("Facet nested isn't here yet")
} else if (mymachine == TRUE) {
	source("/Users/butellyn/Documents/ButlerPlotFuncs/plotFuncs.R")
  source("/Users/butellyn/Documents/ButlerPlotFuncs/facetnested.R")
}
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel', 'DescTools', 'gtable')

# Now load the data
if (galton == TRUE) {
	vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
	cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
	gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
	reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
	alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
	tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')
} else if (mymachine == TRUE) {
	vol.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/meanLR/volumeData.csv')
	cbf.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/meanLR/cbfData.csv')
	gmd.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/meanLR/gmdData.csv')
	reho.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/meanLR/rehoData.csv')
	alff.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/meanLR/alffData.csv')
	tr.modal.data <- read.csv('/Users/butellyn/Documents/hiLo/data/meanLR/jlfTRData.csv')
}

###### Average L & R for nback (weight by vol)
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
nback_df$scanid <- NULL
nback.modal.data <- averageLeftAndRight_WeightByVol(vol_df, nback_df, volString="mprage_jlf_vol", otherString="sigchange_contrast4_2back0back_mean_miccai", rightString="_R_", leftString="_L_", averageString="_ave_")
nback.modal.data <- nback.modal.data[,c("bblid", grep("ave", colnames(nback.modal.data), value=TRUE))]

nback.modal.data <- merge(nback.modal.data, nback_quality_df)
# Merge in rest to get exclusions that were imposed for rest... may be appropriate... told Kosha November 19, 2019
nback.modal.data <- merge(nback.modal.data, alff.modal.data)




##### Average L & R for idemo (weight by vol)
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

vol_df <- vol_df[,c("bblid", grep("mprage_jlf_vol", colnames(vol_df), value=TRUE))]
vol_df <- vol_df[,!(colnames(vol_df) %in% grep("WM", colnames(vol_df), value=TRUE))]
vol_df <- vol_df[,!(colnames(vol_df) %in% paste0("mprage_jlf_vol_", c("ICV", "TBV", "TBGM")))]

idemo_df$idemo_jlf_cope1_Task_Brain_Stem <- NULL
sigchange_vec <- grep("idemo", colnames(idemo_df), value=TRUE)
signames <- c()
for (sigc in sigchange_vec) {
	tmp <- strsplit(sigc, split="_")
	tmp2 <- ""
	for (i in 6:length(tmp[[1]])) {
		if (i > 6) {
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
idemo_df <- idemo_df[order(idemo_df$bblid),]
rownames(idemo_df) <- 1:nrow(idemo_df)

idemo.modal.data <- averageLeftAndRight_WeightByVol(vol_df, idemo_df, volString="mprage_jlf_vol", otherString="idemo_jlf_cope1_Task", rightString="_R_", leftString="_L_", averageString="_ave_")
idemo.modal.data <- idemo.modal.data[,c("bblid", grep("ave", colnames(idemo.modal.data), value=TRUE))]

idemo.modal.data <- merge(idemo.modal.data, idemo_quality_df)

# Merge in vol to get t1Exclude and time range exclusions
idemo.modal.data <- merge(idemo.modal.data, vol.modal.data[,c("bblid", "F1_Exec_Comp_Cog_Accuracy", "sex", "ageAtGo1Scan")])

# Apply idemo exclusion
idemo.modal.data <- idemo.modal.data[idemo.modal.data$idemoExclude == 0,]
rownames(idemo.modal.data) <- 1:nrow(idemo.modal.data)

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
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_', 'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_', 'idemo_jlf_cope1_Task_ave_')
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

  # Create age bins
  tmp_df$ageYrs <- tmp_df$ageAtGo1Scan/12
  tmp_df$ageBin <- NA
  tmp_df[tmp_df$ageYrs < 13, "ageBin"] <- "Children"
  tmp_df[tmp_df$ageYrs >= 13 & tmp_df$ageYrs < 18, "ageBin"] <- "Adolescents"
  tmp_df[tmp_df$ageYrs >= 18, "ageBin"] <- "Young Adults"

	# Filter out Me and rename rows
	tmp_df <- merge(tmp_df, hilo_df)
	tmp_df <- tmp_df[tmp_df$CogGroup != "Me",]
	tmp_df$CogGroup <- factor(tmp_df$CogGroup)
	tmp_df$CogGroup <- factor(tmp_df$CogGroup, levels=c("Lo", "Hi"))

	for (sex in unique(tmp_df$sex)) {
		tmp_df_sex <- tmp_df[tmp_df$sex == sex,]
		tmp_df_sex$age <- scale(tmp_df_sex$ageAtGo1Scan)
		tmp_df_sex$age2 <- scale((tmp_df_sex$age)^2)
		tmp_df_sex$age3 <- scale((tmp_df_sex$age)^3)
		tmp_df_sex <- tmp_df[tmp_df$sex == sex,]
		tmp_df_sex$F1_Exec_Comp_Cog_Accuracy <- scale(tmp_df_sex$F1_Exec_Comp_Cog_Accuracy)
		tmp_df_sex[, qualitymetrics[q]] <- scale(tmp_df_sex[, qualitymetrics[q]])
		tmp_df_sex[, grep(grepVals[q], colnames(tmp_df_sex))] <- scale(tmp_df_sex[, grep(grepVals[q], colnames(tmp_df_sex))])

		rownames(tmp_df_sex) <- 1:nrow(tmp_df_sex)

		assign(paste0(dfName, "_", sex), tmp_df_sex)
	}
}

# Get coefficients for CogGroup
basgang <- c("Thal", "Put", "Cau", "Pall", "Acc")
limbic <- c("PHG", "Hipp", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amy", "MCgG")
front <- c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "PrG", "MSFG", "SMC", "MFG", "SFG")
temporal <- c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG")
parietal <- c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG")
occipital <- c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG")
cerebellum <- c("C1-5", "C6-7", "C8-10", "CExt")
whitematter <- c("IWM", "LWM", "FWM", "TWM", "PWM", "OWM")
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

	sum_df <- data.frame(matrix(NA, nrow=length(colstouse)*6, ncol=7)) ####
	colnames(sum_df) <- c("Label", "Abbrev", "Lobe", "ageBin", "Importance", "Sex", "EffectSize")
  sum_df$Sex <- c(rep("Female", length(colstouse)*3), rep("Male", length(colstouse)*3))

	for (i in 1:length(colstouse)) {
		sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Label"] <- colstouse[i]
    sum_df[c(i, i+length(colstouse)*3), "ageBin"] <- "Children"
    sum_df[c(i+length(colstouse), i+length(colstouse)*4), "ageBin"] <- "Adolescents"
    sum_df[c(i+length(colstouse)*2, i+length(colstouse)*5), "ageBin"] <- "Young Adults"

		intname <- strsplit(colstouse[i], split="_")
		if (q != 7 & q != 8) { thisint = 4
		} else if (q == 7) { thisint = 7
		} else { thisint = 6 }
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
		if (intname == "Cerebellar_Vermal_Lobules_I.V") { intname <- "C1-5"
		} else if (intname == "Cerebellar_Vermal_Lobules_VI.VII") { intname <- "C6-7"
		} else if (intname == "Cerebellar_Vermal_Lobules_VIII.X") { intname <- "C8-10"
		} else if (intname == "Accumbens_Area") { intname <- "Acc"
		} else if (intname == "Amygdala") { intname <- "Amy"
		} else if (intname == "Caudate") { intname <- "Cau"
		} else if (intname == "Cerebellum_Exterior") { intname <- "CExt"
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

		sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Abbrev"] <- intname

		if (intname %in% basgang) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Basal Ganglia"
		} else if (intname %in% limbic) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Limbic"
		} else if (intname %in% front) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Frontal"
		} else if (intname %in% temporal) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Temporal"
		} else if (intname %in% parietal) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Parietal"
		} else if (intname %in% occipital) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Occipital"
		} else if (intname %in% cerebellum) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Cerebellum"
		} else if (intname %in% whitematter) {
      sum_df[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "White Matter"
    }
	}

	for (sex in c("Female", "Male")) {
  	dfName <- paste0(data.names[q], ".modal.data_", sex)
  	tmp_df <- get(dfName)

    for (ageGroup in c("Children", "Adolescents", "Young Adults")) {
    	for (j in 1:length(colstouse)) {
    		func <- paste0(colstouse[j], "~I(CogGroup)+age+age2+age3+", qualitymetrics[q])

    		mod <- lm(as.formula(func), data=tmp_df[tmp_df$ageBin == ageGroup, ])
    		sum_df[sum_df$Sex == sex & sum_df$ageBin == ageGroup & sum_df$Label == colstouse[j], "EffectSize"] <- summary(mod)$coefficients[2, 1]
      }
    }
	}

	PFITregions <- c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG", "ITG", "STG", "Cau", "MOG")

	sum_df$Lobe <- factor(sum_df$Lobe, levels = c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital", "Cerebellum", "White Matter"))
	for (j in 1:nrow(sum_df)) {
		if (sum_df[j, "Abbrev"] %in% PFITregions) {
			sum_df[j, "Importance"] <- "PF"
		} else {
      sum_df[j, "Importance"] <- "Other"
    }
	}

  minscale <- RoundTo(min(sum_df$EffectSize), multiple=.2, FUN=round)
  maxscale <- RoundTo(max(sum_df$EffectSize), multiple=.2, FUN=round)
  if (minscale > min(sum_df$EffectSize)) { minscale <- minscale - .2 }
  if (maxscale < max(sum_df$EffectSize)) { maxscale <- maxscale + .2 }

	if (data.names[q] == "vol") {
		thistit <- "Volume"
	} else if (data.names[q] == "gmd") {
		thistit <- "GMD"
	} else if (data.names[q] == "tr") {
		thistit <- "MD"
	} else if (data.names[q] == "cbf") {
		thistit <- "CBF"
	} else if (data.names[q] == "alff") {
		thistit <- "ALFF"
	} else if (data.names[q] == "reho") {
		thistit <- "ReHo"
	} else if (data.names[q] == "nback") {
		thistit <- "NBack"
	} else if (data.names[q] == "idemo") {
		thistit <- "IDEMO"
	}

	sum_df$Importance <- ordered(sum_df$Importance, levels=c("PF", "Other"))
  sum_df$ageBin <- ordered(sum_df$ageBin, levels=c("Young Adults", "Adolescents", "Children"))

  sum_df$Sex <- factor(sum_df$Sex)

	thisplot <- ggplot(sum_df, aes(Abbrev, EffectSize, group=Sex, colour=Sex)) + geom_point(stat="identity") +
    geom_line() + scale_y_continuous(limits=c(minscale, maxscale), breaks=round(seq(minscale, maxscale, .2), digits=1)) +
    facet_nested(ageBin ~ Lobe + Importance, scales="free", space="free_x") +
    ylab("Effect Size") + theme_linedraw() + geom_hline(yintercept=0) +
    labs(fill = "Sex") + theme(axis.text.x = element_text(angle=90)) +
    ggtitle(thistit) +
    theme(legend.position="bottom", legend.box="vertical", axis.title.x=element_blank(), plot.title=element_text(vjust=1, face="bold"))

	assign(paste0(data.names[q], "_plot"), thisplot)
}

if (galton == TRUE) {
	pdf(paste0('/home/butellyn/hiLo/plots/beta_HiLo_Age_', Sys.Date(), '.pdf'), width=15, height=12)
	grid.arrange(vol_plot, gmd_plot, nrow=2)
  grid.arrange(tr_plot, nback_plot, nrow=2)
  grid.arrange(idemo_plot, cbf_plot, nrow=2)
  grid.arrange(alff_plot, reho_plot, nrow=2)
	dev.off()
} else if (mymachine == TRUE) {
	pdf(paste0('/Users/butellyn/Documents/hiLo/plots/beta_HiLo_Age_', Sys.Date(), '.pdf'), width=15, height=12)
	grid.arrange(vol_plot, gmd_plot, nrow=2)
  grid.arrange(tr_plot, nback_plot, nrow=2)
  grid.arrange(idemo_plot, cbf_plot, nrow=2)
  grid.arrange(alff_plot, reho_plot, nrow=2)
	dev.off()
}
