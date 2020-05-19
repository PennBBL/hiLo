### This script plots age and sex interactions for all modalities, condensing
### age bins into single rows (within age bin age regression added)
###
### Ellyn Butler
### May 11, 2020


# What machine are you working on?
galton=FALSE
mymachine=TRUE

## Load Library(s)
if (galton == TRUE) {
	source("~/ButlerPlotFuncs/plotFuncs.R")
} else if (mymachine == TRUE) {
	source("~/Documents/ButlerPlotFuncs/plotFuncs.R")
}
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling',
	'data.table', 'ggrepel', 'DescTools', 'gtable', 'ggh4x')

# Now load the data
if (galton == TRUE) {
	vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
	cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
	gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
	reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
	alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
	tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')
} else if (mymachine == TRUE) {
	vol.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
	cbf.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/cbfData.csv')
	gmd.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/gmdData.csv')
	reho.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/rehoData.csv')
	alff.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/alffData.csv')
	tr.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/jlfTRData.csv')
	nback.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/nbackData.csv')
	idemo.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/idemoData.csv')
}


######### Create Adon's hi-lo definitions and merge with every dataframe
# ./hiLo/scripts/01_DataPrep/scripts/prepareNBins.R

hilo_df <- vol.modal.data[,c("bblid", "F1_Exec_Comp_Cog_Accuracy")]
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

# Ellyn: scale (0, 1) all cognitive, brain, age and quality data
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_', 'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_', 'sigchange_cope1_task_mean_miccai_ave_')
data.names <- c('vol', 'gmd', 'tr', 'cbf', 'alff', 'reho', 'nback', 'idemo')

qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "nbackRelMeanRMSMotion", "idemoRelMeanRMSMotion")
for (a in 1:8) {
	# Recode sex
	dfName <- paste(data.names[a], ".modal.data", sep='')
	tmp_df <- get(dfName)
	tmp_df[tmp_df$sex == 2, "sex"] <- "Female"
	tmp_df[tmp_df$sex == 1, "sex"] <- "Male"

	# Filter out Me and rename rows
	tmp_df <- merge(tmp_df, hilo_df)
	tmp_df <- tmp_df[tmp_df$CogGroup != "Me",]
	tmp_df$CogGroup <- factor(tmp_df$CogGroup, levels=c("Lo", "Hi"))

	tmp_df$age <- scale(tmp_df$ageAtGo1Scan)
	tmp_df$age2 <- scale((tmp_df$age)^2)
	tmp_df$age3 <- scale((tmp_df$age)^3)
	tmp_df$F1_Exec_Comp_Cog_Accuracy <- scale(tmp_df$F1_Exec_Comp_Cog_Accuracy)
	tmp_df[, qualitymetrics[a]] <- scale(tmp_df[, qualitymetrics[a]])
	tmp_df[, grep(grepVals[a], colnames(tmp_df))] <- scale(tmp_df[, grep(grepVals[a], colnames(tmp_df))])

	assign(dfName, tmp_df)
}

# Get coefficients for CogGroup
basgang <- c("Thalamus_Proper", "Putamen", "Caudate", "Pallidum", "Accumbens_Area")
limbic <- c("PHG", "Hippocampus", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amygdala", "MCgG")
front <- c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "PrG", "MSFG", "SMC", "MFG", "SFG")
temporal <- c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG")
parietal <- c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG")
occipital <- c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG")
cerebellum <- c("Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII",
  "Cerebellar_Vermal_Lobules_VIII.X", "Cerebellum_Exterior")
whitematter <- c("Insular_Lobe_WM", "Limbic_Lobe_WM", "Frontal_Lobe_WM",
	"Temporal_Lobe_WM", "Parietal_Lobe_WM", "Occipital_Lobe_WM")
for (a in 1:8) {
	dfName <- paste0(data.names[a], ".modal.data")
	tmp_df <- get(dfName)

	colstouse <- grep(grepVals[a], colnames(tmp_df), value=TRUE)
	colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]

	sum_df_tmp <- data.frame(matrix(NA, nrow=length(colstouse), ncol=5)) ####
	colnames(sum_df_tmp) <- c("Modality", "Label", "Abbrev", "Lobe", "EffectSize")

	for (i in 1:length(colstouse)) {
		sum_df_tmp[i, "Label"] <- colstouse[i]

		intname <- strsplit(colstouse[i], split="_")
		if (a != 7 & a != 8) { thisint = 4
		} else if (a == 7 | a == 8) { thisint = 7 }

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

		sum_df_tmp[i, "Abbrev"] <- intname

		if (intname %in% basgang) { sum_df_tmp[i, "Lobe"] <- "ST"
		} else if (intname %in% limbic) { sum_df_tmp[i, "Lobe"] <- "Lim"
		} else if (intname %in% front) { sum_df_tmp[i, "Lobe"] <- "Fro"
		} else if (intname %in% temporal) { sum_df_tmp[i, "Lobe"] <- "Tem"
		} else if (intname %in% parietal) { sum_df_tmp[i, "Lobe"] <- "Par"
		} else if (intname %in% occipital) { sum_df_tmp[i, "Lobe"] <- "Occ"
		} else if (intname %in% cerebellum) { sum_df_tmp[i, "Lobe"] <- "Cere"
		} else if (intname %in% whitematter) { sum_df_tmp[i, "Lobe"] <- "WM"
    }
	}

  for (j in 1:length(colstouse)) {
    func <- paste0(colstouse[j], "~I(CogGroup)+age+age2+age3+sex+", qualitymetrics[a])
		mod <- lm(as.formula(func), data=tmp_df)
    sum_df_tmp[sum_df_tmp$Label == colstouse[j], "EffectSize"] <- summary(mod)$coefficients[2, 1]
  }

	if (data.names[a] == "vol") {
		sum_df_tmp$Modality <- "Volume"
	} else if (data.names[a] == "gmd") {
		sum_df_tmp$Modality <- "GMD"
	} else if (data.names[a] == "tr") {
		sum_df_tmp$Modality <- "MD"
	} else if (data.names[a] == "cbf") {
		sum_df_tmp$Modality <- "CBF"
	} else if (data.names[a] == "alff") {
		sum_df_tmp$Modality <- "ALFF"
	} else if (data.names[a] == "reho") {
		sum_df_tmp$Modality <- "ReHo"
	} else if (data.names[a] == "nback") {
		sum_df_tmp$Modality <- "NBack"
	} else if (data.names[a] == "idemo") {
		sum_df_tmp$Modality <- "IdEmo"
	}

	if (a == 1) {
		sum_df <- sum_df_tmp
	} else {
		sum_df <- rbind(sum_df, sum_df_tmp)
	}
}


sum_df$Lobe <- factor(sum_df$Lobe, levels = c("Fro", "Lim", "Occ", "Par", "ST", "Tem", "WM", "Cere"))
sum_df$Modality <- factor(sum_df$Modality, levels = c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "NBack", "IdEmo"))
sum_df <- arrange(sum_df, Lobe, Abbrev)

sum_df <- sum_df[!(sum_df$Lobe %in% c("Cere", "WM")),]

sum_df2 <- data.frame(Section=sum_df[sum_df$Modality=="Volume", "Lobe"],
	ROI=sum_df[sum_df$Modality=="Volume", "Abbrev"],
	Volume=sum_df[sum_df$Modality=="Volume", "EffectSize"],
	GMD=sum_df[sum_df$Modality=="GMD", "EffectSize"],
	MD=sum_df[sum_df$Modality=="MD", "EffectSize"],
	CBF=sum_df[sum_df$Modality=="CBF", "EffectSize"],
	ALFF=sum_df[sum_df$Modality=="ALFF", "EffectSize"],
	ReHo=sum_df[sum_df$Modality=="ReHo", "EffectSize"],
	NBack=sum_df[sum_df$Modality=="NBack", "EffectSize"],
	IdEmo=sum_df[sum_df$Modality=="IdEmo", "EffectSize"])

write.csv(sum_df2, file="~/Documents/hiLo/data/effsizes/scaledCoeffCombinedSex.csv", row.names=FALSE)



### Compare with Kosha
