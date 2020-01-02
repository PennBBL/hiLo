## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')
source("/home/butellyn/ButlerPlotFuncs/plotFuncs_AdonPath.R")

# Now load the data
vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

# Average L & R for nback (weight by vol)
vol_df <- read.csv("/home/butellyn/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")

nback_df <- read.csv('/home/butellyn/hiLo/data/n2416_nback2minus0_20191118.csv')
nback_quality_df <- read.csv('/home/butellyn/hiLo/data/n2416_nback2minus0_quality_20191118.csv')

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


# Ellyn: scale (0, 1) all cognitive, brain, age and quality data
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_', 'rest_jlf_alff_', 'rest_jlf_reho_', 'sigchange_contrast4_2back0back_mean_miccai_ave_')
data.names <- c('vol', 'gmd', 'tr', 'cbf', 'alff', 'reho', 'nback')
whitematterValues <- c(1, 0, 1, 1, 0, 0, 0)
cerebellumValues <- c(1, 1, 1, 0, 1, 1, 0)
qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion", "relMeanRMSMotion")
for (q in 1:7) {
	# Recode sex
	dfName <- paste(data.names[q], ".modal.data", sep='')
	tmp_df <- get(dfName)
	tmp_df[tmp_df$sex == 2, "sex"] <- "Female"
	tmp_df[tmp_df$sex == 1, "sex"] <- "Male"
	for (sex in unique(tmp_df$sex)) {
		tmp_df_sex <- tmp_df[tmp_df$sex == sex,]
		tmp_df_sex$age <- scale(tmp_df_sex$ageAtGo1Scan)
		tmp_df_sex$age2 <- scale((tmp_df_sex$age)^2)
		tmp_df_sex$age3 <- scale((tmp_df_sex$age)^3)
		tmp_df_sex$F1_Exec_Comp_Cog_Accuracy <- scale(tmp_df_sex$F1_Exec_Comp_Cog_Accuracy)
		tmp_df_sex[, qualitymetrics[q]] <- scale(tmp_df_sex[, qualitymetrics[q]])
		tmp_df_sex[, grep(grepVals[q], colnames(tmp_df_sex))] <- scale(tmp_df_sex[, grep(grepVals[q], colnames(tmp_df_sex))])
		assign(paste0(dfName, "_", sex), tmp_df_sex)
	}
}




thistext <- paste0("Figure 1. Coefficients for the standardized\nComplex Cognition Accuracy factor score for each region.\nRegion ~ CompCogAcc + Age + Age^2 + Age^3 + QA\n")

# Get coefficients for F1_Exec_Comp_Cog_Accuracy
basgang <- c("Thal", "Put", "Cau", "Pall", "Acc", "INS_WM")
limbic <- c("PHG", "Hipp", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amy", "MCgG", "LIM_WM")
frontorb <- c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "FRO_WM")
frontdors <- c("PrG", "MSFG", "SMC", "MFG", "SFG")
temporal <- c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG", "TMP_WM")
parietal <- c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG", "PAR_WM")
occipital <- c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG", "OCC_WM")
cerebellum <- c("Cer_1-5", "Cer_6-7", "Cer_8-10", "Cer_Ext")
for (q in 1:7) {
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

	sum_df <- data.frame(matrix(NA, nrow=length(colstouse), ncol=9))
	colnames(sum_df) <- c("Label", "Abbrev", "Abbrev2", "Lobe", "Importance", "Female", "Male", "Female_SE", "Male_SE")

	for (i in 1:length(colstouse)) {
		sum_df[i, "Label"] <- colstouse[i]
		intname <- strsplit(colstouse[i], split="_")
		if (q != 7) { thisint = 4
		} else { thisint = 7 }
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
		} else if (intname == "Limbic_Lobe_WM") { intname <- "LIM_WM"
		} else if (intname == "Insular_Lobe_WM") { intname <- "INS_WM"
		} else if (intname == "Frontal_Lobe_WM") { intname <- "FRO_WM"
		} else if (intname == "Parietal_Lobe_WM") { intname <- "PAR_WM"
		} else if (intname == "Occipital_Lobe_WM") { intname <- "OCC_WM"
		} else if (intname == "Temporal_Lobe_WM") { intname <- "TMP_WM"
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

	for (sex in c("Female", "Male")) {
		dfName <- paste0(data.names[q], ".modal.data_", sex)
		tmp_df <- get(dfName)
		for (j in 1:length(colstouse)) {
			func <- paste0(colstouse[j], "~F1_Exec_Comp_Cog_Accuracy+age+age2+age3+", qualitymetrics[q])
			mod <- lm(as.formula(func), data=tmp_df)

			sum_df[j, sex] <- summary(mod)$coefficients[2, 1]
			sum_df[j, paste0(sex, "_SE")] <- summary(mod)$coefficients[2, 2]
		}
	}

	thisSE <- max(c(sum_df$Female_SE, sum_df$Male_SE))

	PFITregions <- c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG", "ITG", "STG", "Cau", "MOG")
	
	sum_df$Lobe <- factor(sum_df$Lobe, levels = c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital", "Cerebellum"))
	for (j in 1:nrow(sum_df)) {
		if (sum_df[j, "Abbrev"] %in% PFITregions) {
			sum_df[j, "Importance"] <- "PFIT"
			sum_df[j, "Abbrev2"] <- sum_df[j, "Abbrev"]
		} 
	}

	pfitters <- c(abs(sum_df[sum_df$Importance == "PFIT", "Female"]), abs(sum_df[sum_df$Importance == "PFIT", "Male"]))
	pfitters <- pfitters[!is.na(pfitters)]
	topfour <- rev(pfitters[order(pfitters)])[1:4]

	for (j in 1:nrow(sum_df)) {
		if (abs(sum_df[j, "Female"]) > 2*sum_df[j, "Female_SE"] & abs(sum_df[j, "Male"]) > 2*sum_df[j, "Male_SE"] & (!(sum_df[j, "Abbrev"] %in% PFITregions)) & (abs(sum_df[j, "Female"]) > min(topfour)| abs(sum_df[j, "Male"]) > min(topfour))) {
			sum_df[j, "Importance"] <- "Major" 
			sum_df[j, "Abbrev2"] <- sum_df[j, "Abbrev"]
		} else if (!(sum_df[j, "Abbrev"] %in% PFITregions)) {
			sum_df[j, "Importance"] <- "None" 
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

	F_df <- get(paste0(data.names[q], ".modal.data_Female"))
	M_df <- get(paste0(data.names[q], ".modal.data_Male"))

	if (data.names[q] == "vol") {
		thistit <- "Volume: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "Volume: M N=", nrow(M_df), ", F N=", nrow(F_df), "; ")
	} else if (data.names[q] == "gmd") {
		thistit <- "Gray Matter Density: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "Gray Matter Density: M N=", nrow(M_df), ", F N=", nrow(F_df), "\n")
	} else if (data.names[q] == "tr") {
		thistit <- "Mean Diffusivity: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "Mean Diffusivity: M N=", nrow(M_df), ", F N=", nrow(F_df), "; ")
	} else if (data.names[q] == "cbf") {
		thistit <- "Cerebral Blood Flow: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "Cerebral Blood Flow: M N=", nrow(M_df), ", F N=", nrow(F_df), "\n")
	} else if (data.names[q] == "alff") {
		thistit <- "Amp of Low Freq Fluc: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "Amp of Low Freq Fluc: M N=", nrow(M_df), ", F N=", nrow(F_df), "; ")
	} else if (data.names[q] == "reho") {
		thistit <- "Regional Homogeneity: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "Regional Homogeneity: M N=", nrow(M_df), ", F N=", nrow(F_df), "\n")
	} else if (data.names[q] == "nback") {
		thistit <- "2 - 0 Back: Coefficient for Complex Cognition"
		thistext <- paste0(thistext, "2 - 0 Back: M N=", nrow(M_df), ", F N=", nrow(F_df))
	}
	
	sum_df$Importance <- ordered(sum_df$Importance, levels=c("None", "Major", "PFIT"))

	lowerbound <- force(-thisSE*2)
	upperbound <- force(thisSE*2)

	minscale <- round(min(sum_df$Female, sum_df$Male), digits=1) ####
	maxscale <- round(max(sum_df$Female, sum_df$Male), digits=1) ####
	if (minscale > min(c(sum_df$Female, sum_df$Male, lowerbound))) { 
		minscale <- round(min(c(sum_df$Female, sum_df$Male, lowerbound)), digits=1) - .05
	}
	if (maxscale < max(c(sum_df$Female, sum_df$Male, upperbound))) { 
		maxscale <- round(max(c(sum_df$Female, sum_df$Male, upperbound)), digits=2) + .05 
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
		ggplot(sum_df, aes(x=Male, y=Female)) + theme_minimal() + 
		geom_rect(aes(xmin=lowerbound, xmax=upperbound, ymin=-Inf, ymax=Inf), fill="grey97", color=NA, alpha=.2, size=0) +
		geom_rect(aes(ymin=lowerbound, ymax=upperbound, xmin=-Inf, xmax=Inf), fill="grey97", color=NA, alpha=.2, size=0) +
		geom_point(aes(color=Importance, shape=Importance), size=2) +  ####
		geom_text_repel(aes(label=Abbrev2, color=Importance), show.legend=FALSE, size=6, nudge_x=c(minscale/5, maxscale/5), nudge_y=c(minscale/8, maxscale/8)) + ####
		guides(text_repel=FALSE) + scale_shape_manual(values=theseshapes) + ####
		scale_color_manual(values=thesecolors) + labs(title=thistit) + 
		xlab("Male Beta (95% CI)") + ylab("Female Beta (95% CI)") +
		geom_abline(intercept=0, slope=1, linetype="dotdash", alpha=.5) + geom_smooth(method="lm", formula=y~x, se=FALSE, color="red") + #might remove 
		geom_hline(yintercept=0, linetype="dashed", color="blue") + geom_vline(xintercept=0, linetype="dashed", color="blue") +
		geom_hline(yintercept=thisSE*2, linetype="dashed", color="slateblue") + geom_vline(xintercept=thisSE*2, linetype="dashed", color="slateblue") +
		geom_hline(yintercept=-thisSE*2, linetype="dashed", color="slateblue") + geom_vline(xintercept=-thisSE*2, linetype="dashed", color="slateblue") + 
		annotate(geom="text", x=minscale+(maxscale-minscale)/8, y=maxscale, size=6, label=paste0("r = ", round(cor(sum_df$Female, sum_df$Male), digits=3))) +
		theme(plot.title = element_text(size=15, face="bold"), axis.title = element_text(size=15), axis.text = element_text(size=15), axis.text.x = element_text(angle = 90))+ 
		scale_x_continuous(limits=c(minscale, maxscale), breaks=round(seq(minscale, maxscale, .05), digits=2)) + ####
		scale_y_continuous(limits=c(minscale, maxscale), breaks=round(seq(minscale, maxscale, .05), digits=2))  ####
	}
	
	outPlot <- plot_fun(sum_df, lowerbound, upperbound, thesecolors)

	assign(paste0(data.names[q], "_plot"), outPlot)
}

obj <- grid.text(thistext)

pdf(paste0('/home/butellyn/hiLo/plots/beta_CompCogAcc_Ruben_', Sys.Date(), '.pdf'), width=27, height=12)
grid.arrange(obj, vol_plot, gmd_plot, tr_plot, cbf_plot, alff_plot, reho_plot, nback_plot, nrow=2, ncol=4)
dev.off()










