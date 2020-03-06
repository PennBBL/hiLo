### This script plots age and sex interactions for all modalities, condensing
### age bins into single rows (within age bin age regression added)
###
### Ellyn Butler
### December 12, 2019 - March 3, 2020


# What machine are you working on?
galton=FALSE
mymachine=TRUE

## Load Library(s)
if (galton == TRUE) {
	source("/home/butellyn/ButlerPlotFuncs/plotFuncs.R")
  stop("Facet nested isn't here yet")
} else if (mymachine == TRUE) {
	source("~/Documents/ButlerPlotFuncs/plotFuncs.R")
  source("~/Documents/ButlerPlotFuncs/facetnested.R")
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
	vol.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')
	cbf.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/cbfData.csv')
	gmd.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/gmdData.csv')
	reho.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/rehoData.csv')
	alff.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/alffData.csv')
	tr.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/jlfTRData.csv')
	nback.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/nbackData.csv')
	idemo.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/idemoData.csv')
	fa.modal.data <- read.csv('~/Documents/hiLo/data/meanLR/jhuFATracts.csv')
}

fa.modal.data <- fa.modal.data[, c("bblid", "sex", "ageAtGo1Scan", "dti64Tsnr", grep("dti_dtitk_jhutract_fa_", colnames(fa.modal.data), value=TRUE))]

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
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_', 'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_', 'sigchange_cope1_task_mean_miccai_ave_', 'dti_dtitk_jhutract_fa_')
data.names <- c('vol', 'gmd', 'tr', 'cbf', 'alff', 'reho', 'nback', 'idemo', 'fa')
#whitematterValues <- c(1, 0, 1, 1, 0, 0, 0, 0)
#cerebellumValues <- c(1, 1, 1, 0, 1, 1, 0, 0)
whitematterValues <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
cerebellumValues <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "nbackRelMeanRMSMotion", "idemoRelMeanRMSMotion", "dti64Tsnr")
for (a in 1:9) {
	# Recode sex
	dfName <- paste(data.names[a], ".modal.data", sep='')
	tmp_df <- get(dfName)
	tmp_df[tmp_df$sex == 2, "sex"] <- "Female"
	tmp_df[tmp_df$sex == 1, "sex"] <- "Male"

  # Create age bins
  tmp_df$ageYrs <- tmp_df$ageAtGo1Scan/12
  tmp_df$ageBin <- NA
  tmp_df[tmp_df$ageYrs < 13, "ageBin"] <- "Children"
  tmp_df[tmp_df$ageYrs >= 13 & tmp_df$ageYrs < 18, "ageBin"] <- "Adolescents"
  tmp_df[tmp_df$ageYrs >= 18, "ageBin"] <- "Adults"

	# Filter out Me and rename rows
	tmp_df <- merge(tmp_df, hilo_df)
	tmp_df <- tmp_df[tmp_df$CogGroup != "Me",]
	tmp_df$CogGroup <- factor(tmp_df$CogGroup, levels=c("Lo", "Hi"))

	for (sex in unique(tmp_df$sex)) {
		tmp_df_sex <- tmp_df[tmp_df$sex == sex,]
		tmp_df_sex$age <- scale(tmp_df_sex$ageAtGo1Scan)
		tmp_df_sex$age2 <- scale((tmp_df_sex$age)^2)
		tmp_df_sex$age3 <- scale((tmp_df_sex$age)^3)
		tmp_df_sex$F1_Exec_Comp_Cog_Accuracy <- scale(tmp_df_sex$F1_Exec_Comp_Cog_Accuracy)
		tmp_df_sex[, qualitymetrics[a]] <- scale(tmp_df_sex[, qualitymetrics[a]])
		tmp_df_sex[, grep(grepVals[a], colnames(tmp_df_sex))] <- scale(tmp_df_sex[, grep(grepVals[a], colnames(tmp_df_sex))])

		rownames(tmp_df_sex) <- 1:nrow(tmp_df_sex)

		assign(paste0(dfName, "_", sex), tmp_df_sex)
	}
}

# Get coefficients for CogGroup
basgang <- c("Thal", "Put", "Cau", "Pall", "NA")
limbic <- c("PHG", "Hipp", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amy", "MCgG")
front <- c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "PrG", "MSFG", "SMC", "MFG", "SFG")
temporal <- c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG")
parietal <- c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG")
occipital <- c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG")
cerebellum <- c("C1-5", "C6-7", "C8-10", "CExt")
whitematter <- c("Ins", "Lim", "Fro", "Tmp", "Par", "Occ", "ATR", "CGC", "CGH",
	"CST", "FOmn", "FOmj", "IFO", "ILF", "SLF", "UF")
for (a in 1:9) {
	dfName <- paste0(data.names[a], ".modal.data")
	tmp_df <- get(dfName)

	colstouse <- grep(grepVals[a], colnames(tmp_df), value=TRUE)
	colstouse <- colstouse[!(colstouse %in% grep("Vent", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Brain", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("ICV", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("CSF", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Cerebellum_White_Matter", colstouse, value=TRUE))]
	colstouse <- colstouse[!(colstouse %in% grep("Mean", colstouse, value=TRUE))]

	sum_df_tmp <- data.frame(matrix(NA, nrow=length(colstouse)*6, ncol=9)) ####
	colnames(sum_df_tmp) <- c("Modality", "Label", "Abbrev", "Lobe", "ageBin", "Importance", "Sex", "Group", "EffectSize")
  sum_df_tmp$Sex <- c(rep("Female", length(colstouse)*3), rep("Male", length(colstouse)*3))

	for (i in 1:length(colstouse)) {
		sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Label"] <- colstouse[i]
    sum_df_tmp[c(i, i+length(colstouse)*3), "ageBin"] <- "Children"
    sum_df_tmp[c(i+length(colstouse), i+length(colstouse)*4), "ageBin"] <- "Adolescents"
    sum_df_tmp[c(i+length(colstouse)*2, i+length(colstouse)*5), "ageBin"] <- "Adults"

		intname <- strsplit(colstouse[i], split="_")
		if (a != 7 & a != 8 & a != 9) { thisint = 4
		} else if (a == 9) { thisint = 5
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
		if (intname == "Cerebellar_Vermal_Lobules_I.V") { intname <- "C1-5"
		} else if (intname == "Cerebellar_Vermal_Lobules_VI.VII") { intname <- "C6-7"
		} else if (intname == "Cerebellar_Vermal_Lobules_VIII.X") { intname <- "C8-10"
		} else if (intname == "Accumbens_Area") { intname <- "NA"
		} else if (intname == "Amygdala") { intname <- "Amy"
		} else if (intname == "Caudate") { intname <- "Cau"
		} else if (intname == "Cerebellum_Exterior") { intname <- "CExt"
		} else if (intname == "Hippocampus") { intname <- "Hipp"
		} else if (intname == "Pallidum") { intname <- "Pall"
		} else if (intname == "Putamen") { intname <- "Put"
		} else if (intname == "Thalamus_Proper") { intname <- "Thal"
		} else if (intname == "Limbic_Lobe_WM") { intname <- "Lim"
		} else if (intname == "Insular_Lobe_WM") { intname <- "Ins"
		} else if (intname == "Frontal_Lobe_WM") { intname <- "Fro"
		} else if (intname == "Parietal_Lobe_WM") { intname <- "Par"
		} else if (intname == "Occipital_Lobe_WM") { intname <- "Occ"
		} else if (intname == "Temporal_Lobe_WM") { intname <- "Tmp"
		} else if (intname == "atr") { intname <- "ATR"
		} else if (intname == "cgc") { intname <- "CGC"
		} else if (intname == "cgh") { intname <- "CGH"
		} else if (intname == "cst") { intname <- "CST"
		} else if (intname == "forceps_minor") { intname <- "FOmn"
		} else if (intname == "forceps_mjor") { intname <- "FOmj"
		} else if (intname == "ifo") { intname <- "IFO"
		} else if (intname == "ilf") { intname <- "ILF"
		} else if (intname == "slf") { intname <- "SLF"
		} else if (intname == "uf") { intname <- "UF"
		}

		sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Abbrev"] <- intname

		if (intname %in% basgang) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Basal Ganglia"
		} else if (intname %in% limbic) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Limbic"
		} else if (intname %in% front) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Frontal"
		} else if (intname %in% temporal) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Temporal"
		} else if (intname %in% parietal) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Parietal"
		} else if (intname %in% occipital) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Occipital"
		} else if (intname %in% cerebellum) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "Cerebellum"
		} else if (intname %in% whitematter) {
      sum_df_tmp[seq(i, i+length(colstouse)*5, length(colstouse)), "Lobe"] <- "White Matter"
    }
	}
	for (sex in c("Female", "Male")) {
  	dfName <- paste0(data.names[a], ".modal.data_", sex)
  	temp_df <- get(dfName)

    for (ageGroup in c("Children", "Adolescents", "Adults")) {
    	for (j in 1:length(colstouse)) {
    		func <- paste0(colstouse[j], "~I(CogGroup)+age+age2+age3+", qualitymetrics[a])

    		mod <- lm(as.formula(func), data=temp_df[temp_df$ageBin == ageGroup, ])
    		sum_df_tmp[sum_df_tmp$Sex == sex & sum_df_tmp$ageBin == ageGroup & sum_df_tmp$Label == colstouse[j], "EffectSize"] <- summary(mod)$coefficients[2, 1]
      }
    }
	}

	PFITregions <- c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG", "ITG", "STG", "Cau", "MOG")

	for (j in 1:nrow(sum_df_tmp)) {
		if (sum_df_tmp[j, "Abbrev"] %in% PFITregions) {
			sum_df_tmp[j, "Importance"] <- "PF"
		} else {
      sum_df_tmp[j, "Importance"] <- "Not"
    }
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
	} else if (data.names[a] == "fa") {
		sum_df_tmp$Modality <- "FA"
	}

	write.csv(sum_df_tmp[,c("Modality", "Abbrev", "Lobe", "ageBin", "Importance", "Sex", "EffectSize")],
		file=paste0("/Users/butellyn/Documents/hiLo/data/effsizes/", data.names[a], ".csv"), row.names=FALSE)

	# Limit regions to those picked by adult volume
	if (data.names[a] == "vol") {
		abbrevtokeep <- c()
		for (lobe in c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital")) {
			pick_df <- sum_df_tmp[sum_df_tmp$ageBin == "Adults" & sum_df_tmp$Lobe == lobe & sum_df_tmp$Importance == "Not",]
			pick_F_df <- pick_df[pick_df$Sex == "Female",]
			pick_M_df <- pick_df[pick_df$Sex == "Male",]
			rownames(pick_F_df) <- 1:nrow(pick_F_df)
			rownames(pick_M_df) <- 1:nrow(pick_M_df)

			pick_F_df$MeanFM <- rowMeans(cbind(pick_F_df$EffectSize, pick_M_df$EffectSize))

			if (lobe %in% c("Basal Ganglia", "Limbic", "Parietal", "Occipital", "Cer", "White")) { numother <- 2
			} else { numother <- 3 }
			effsizes <- rev(order(abs(pick_F_df$MeanFM)))[1:numother]

			abbrevtokeep <- c(abbrevtokeep, pick_F_df[effsizes, "Abbrev"])
		}
		abbrevtokeep <- c(abbrevtokeep, "C1-5", "C6-7", "C8-10", "CExt", "Lim", "Ins",
			"Fro", "Occ", "Tmp", "Par", "ATR", "CGC", "CGH", "CST", "FOmn", "FOmj",
			"IFO", "ILF", "SLF", "UF")
	}

	sum_df_tmp <- sum_df_tmp[sum_df_tmp$Importance == "PF" | sum_df_tmp$Abbrev %in% abbrevtokeep,]
	rownames(sum_df_tmp) <- 1:nrow(sum_df_tmp)

	if (a == 1) {
		sum_df <- sum_df_tmp
	} else {
		sum_df <- rbind(sum_df, sum_df_tmp)
	}
}


sum_df$Lobe <- factor(sum_df$Lobe, levels = c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital", "Cerebellum", "White Matter"))
sum_df$Modality <- factor(sum_df$Modality, levels = c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "FA"))
sum_df$Importance <- ordered(sum_df$Importance, levels=c("PF", "Not"))
sum_df$Group <- paste(sum_df$Sex, sum_df$ageBin)
sum_df$Group <- ordered(sum_df$Group, levels=c("Female Children", "Female Adolescents", "Female Adults", "Male Children", "Male Adolescents", "Male Adults"))
sum_df$Sex <- factor(sum_df$Sex)

struc_plot <- ggplot(sum_df[sum_df$Modality %in% c("Volume", "GMD", "MD") & sum_df$Lobe %in% c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital"), ],
		aes(Abbrev, EffectSize, group=Group, colour=Group, fill=Group)) +
	geom_bar(stat="identity", position="dodge") + scale_y_continuous(limits=c(-1.5, 1.5), breaks=round(seq(-1.5, 1.5, .5), digits=1)) +
	facet_nested(Modality ~ Lobe + Importance, scales="free", space="free_x") +
	scale_shape_manual(values=c(17, 15, 16, 17, 15, 16)) +
	ylab("Effect Size") + theme_linedraw() + geom_hline(yintercept=0) +
	geom_hline(yintercept=-.4, linetype="dashed") + geom_hline(yintercept=.4, linetype="dashed") +
	geom_hline(yintercept=-.2, linetype="dashed", color="grey") + geom_hline(yintercept=.2, linetype="dashed", color="grey") +
	labs(fill = "Group") + theme(axis.text.x = element_text(angle=90)) +
	theme(legend.position="bottom", legend.box="vertical", axis.title.x=element_blank()) +
	scale_color_manual(values=c("black", "black", "black", "black", "black", "black"),
			guide = guide_legend(nrow=1)) +
	scale_fill_manual(values=c("pink", "violetred1", "red3", "lightsteelblue1", "steelblue2", "blue4"),
			guide = guide_legend(nrow=1))

func_plot <- ggplot(sum_df[sum_df$Modality %in% c("CBF", "ALFF", "ReHo") & sum_df$Lobe %in% c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital"), ],
		aes(Abbrev, EffectSize, group=Group, colour=Group, fill=Group)) +
	geom_bar(stat="identity", position="dodge") + scale_y_continuous(limits=c(-1, 1), breaks=round(seq(-1, 1, .2), digits=1)) +
	facet_nested(Modality ~ Lobe + Importance, scales="free", space="free_x") +
	scale_shape_manual(values=c(17, 15, 16, 17, 15, 16)) +
	scale_color_manual(values=c("black", "black", "black", "black", "black", "black"), guide = guide_legend(nrow=1)) +
	scale_fill_manual(values=c("pink", "violetred1", "red3", "lightsteelblue1", "steelblue2", "blue4"), guide = guide_legend(nrow=1)) +
	ylab("Effect Size") + theme_linedraw() + geom_hline(yintercept=0) +
	geom_hline(yintercept=-.4, linetype="dashed") + geom_hline(yintercept=.4, linetype="dashed") +
	geom_hline(yintercept=-.2, linetype="dashed", color="grey") + geom_hline(yintercept=.2, linetype="dashed", color="grey") +
	labs(fill = "Group") + theme(axis.text.x = element_text(angle=90)) +
	theme(legend.position="bottom", legend.box="vertical", axis.title.x=element_blank())

task_plot <- ggplot(sum_df[sum_df$Modality %in% c("NBack", "IdEmo") & sum_df$Lobe %in% c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital"), ],
		aes(Abbrev, EffectSize, group=Group, colour=Group, fill=Group)) +
	geom_bar(stat="identity", position="dodge") + scale_y_continuous(limits=c(-.8, 1.2), breaks=round(seq(-1.4, 1.4, .2), digits=1)) +
	facet_nested(Modality ~ Lobe + Importance, scales="free", space="free_x") +
	scale_shape_manual(values=c(17, 15, 16, 17, 15, 16)) +
	scale_color_manual(values=c("black", "black", "black", "black", "black", "black"), guide = guide_legend(nrow=1)) +
	scale_fill_manual(values=c("pink", "violetred1", "red3", "lightsteelblue1", "steelblue2", "blue4"), guide = guide_legend(nrow=1)) +
	ylab("Effect Size") + theme_linedraw() + geom_hline(yintercept=0) +
	geom_hline(yintercept=-.4, linetype="dashed") + geom_hline(yintercept=.4, linetype="dashed") +
	geom_hline(yintercept=-.2, linetype="dashed", color="grey") + geom_hline(yintercept=.2, linetype="dashed", color="grey") +
	labs(fill = "Group") + theme(axis.text.x = element_text(angle=90)) +
	theme(legend.position="bottom", legend.box="vertical", axis.title.x=element_blank())

boo <- sum_df[sum_df$Lobe %in% c("Cerebellum", "White Matter") & sum_df$Modality != "FA", ]
boo$Modality <- as.character(boo$Modality)
boo$Modality <- gsub("NBack", "NB", boo$Modality)
boo$Modality <- gsub("IdEmo", "Id", boo$Modality)
boo$Modality <- factor(boo$Modality)
boo$Modality <- factor(boo$Modality, levels = c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "NB", "Id"))
wmcer_plot <- ggplot(boo, aes(Abbrev, EffectSize, group=Group, colour=Group, fill=Group)) +
	geom_bar(stat="identity", position="dodge") + scale_y_continuous(limits=c(-.8, 1.2), breaks=round(seq(-1.4, 1.4, .2), digits=1)) +
	facet_nested(. ~ Lobe + Modality, scales="free", space="free_x") +
	scale_shape_manual(values=c(17, 15, 16, 17, 15, 16)) +
	scale_color_manual(values=c("black", "black", "black", "black", "black", "black"), guide = guide_legend(nrow=1)) +
	scale_fill_manual(values=c("pink", "violetred1", "red3", "lightsteelblue1", "steelblue2", "blue4"), guide = guide_legend(nrow=1)) +
	ylab("Effect Size") + theme_linedraw() + geom_hline(yintercept=0) +
	geom_hline(yintercept=-.4, linetype="dashed") + geom_hline(yintercept=.4, linetype="dashed") +
	geom_hline(yintercept=-.2, linetype="dashed", color="grey") + geom_hline(yintercept=.2, linetype="dashed", color="grey") +
	labs(fill = "Group") + theme(axis.text.x = element_text(angle=90)) +
	theme(legend.position="bottom", legend.box="vertical", axis.title.x=element_blank())

fa_plot <- ggplot(sum_df[sum_df$Modality == "FA", ], aes(Abbrev, EffectSize, group=Group, colour=Group, fill=Group)) +
	geom_bar(stat="identity", position="dodge") + scale_y_continuous(limits=c(-.8, .8), breaks=round(seq(-1.4, 1.4, .2), digits=1)) +
	facet_nested(Modality ~ Importance, scales="free", space="free_x") +
	scale_shape_manual(values=c(17, 15, 16, 17, 15, 16)) +
	scale_color_manual(values=c("black", "black", "black", "black", "black", "black"), guide = guide_legend(ncol=1)) +
	scale_fill_manual(values=c("pink", "violetred1", "red3", "lightsteelblue1", "steelblue2", "blue4"), guide = guide_legend(ncol=1)) +
	ylab("Effect Size") + theme_linedraw() + geom_hline(yintercept=0) +
	geom_hline(yintercept=-.4, linetype="dashed") + geom_hline(yintercept=.4, linetype="dashed") +
	geom_hline(yintercept=-.2, linetype="dashed", color="grey") + geom_hline(yintercept=.2, linetype="dashed", color="grey") +
	labs(fill = "Group") + theme(axis.text.x = element_text(angle=90)) +
	theme(legend.box="vertical", axis.title.x=element_blank(), legend.title = element_text(size=8), legend.text = element_text(size=8))

if (galton == TRUE) {
	pdf(paste0('/home/butellyn/hiLo/plots/beta_HiLo_Age_condensed_bars_struc_', Sys.Date(), '.pdf'), width=9, height=7)
	struc_plot
	dev.off()

	pdf(paste0('/home/butellyn/hiLo/plots/beta_HiLo_Age_condensed_bars_func_', Sys.Date(), '.pdf'), width=9, height=7)
	func_plot
	dev.off()

	pdf(paste0('/home/butellyn/hiLo/plots/beta_HiLo_Age_condensed_bars_task_', Sys.Date(), '.pdf'), width=9, height=7)
	task_plot
	dev.off()

	pdf(paste0('/home/butellyn/hiLo/plots/beta_HiLo_Age_condensed_bars_wmcer_', Sys.Date(), '.pdf'), width=12, height=5)
	wmcer_plot
	dev.off()
} else if (mymachine == TRUE) {
	pdf(paste0('/Users/butellyn/Documents/hiLo/plots/beta_HiLo_Age_condensed_bars_struc_', Sys.Date(), '.pdf'), width=9, height=7)
	struc_plot
	dev.off()

	pdf(paste0('/Users/butellyn/Documents/hiLo/plots/beta_HiLo_Age_condensed_bars_func_', Sys.Date(), '.pdf'), width=9, height=7)
	func_plot
	dev.off()

	pdf(paste0('/Users/butellyn/Documents/hiLo/plots/beta_HiLo_Age_condensed_bars_task_', Sys.Date(), '.pdf'), width=9, height=7)
	task_plot
	dev.off()

	pdf(paste0('/Users/butellyn/Documents/hiLo/plots/beta_HiLo_Age_condensed_bars_wmcer_', Sys.Date(), '.pdf'), width=14, height=5)
	wmcer_plot
	dev.off()

	png('/Users/butellyn/Documents/hiLo/plots/figure3_color.png', units="mm", width=220, height=150, res=800)
	struc_plot
	dev.off()

	png('/Users/butellyn/Documents/hiLo/plots/figure4_color.png', units="mm", width=220, height=150, res=800)
	func_plot
	dev.off()

	png('/Users/butellyn/Documents/hiLo/plots/figure5_color.png', units="mm", width=220, height=150, res=800)
	task_plot
	dev.off()

	png('/Users/butellyn/Documents/hiLo/plots/figure6_color.png', units="mm", width=220, height=100, res=800)
	wmcer_plot
	dev.off()

	png('/Users/butellyn/Documents/hiLo/plots/fa_color.png', units="mm", width=140, height=60, res=800)
	fa_plot
	dev.off()
}
