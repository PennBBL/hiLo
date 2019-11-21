## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

# Now load the data
vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')



# Ellyn: scale (0, 1) all cognitive, brain, age and quality data
grepVals <- c('mprage_jlf_vol_', 'mprage_jlf_gmd_', 'dti_jlf_tr_', 'pcasl_jlf_cbf_', 'rest_jlf_alff_', 'rest_jlf_reho_')
data.names <- c('vol', 'gmd', 'tr', 'cbf', 'alff', 'reho')
whitematterValues <- c(1, 0, 1, 1, 0, 0)
cerebellumValues <- c(1, 1, 1, 0, 1, 1)
qualitymetrics <- c("averageManualRating", "averageManualRating", "dti64Tsnr", "pcaslRelMeanRMSMotion", "restRelMeanRMSMotion", "restRelMeanRMSMotion")
for (q in 1:6) {
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

# Get coefficients for F1_Exec_Comp_Cog_Accuracy
basgang <- c("Thal", "Put", "Cau", "Pall", "Acc", "INS WM")
limbic <- c("PHG", "Hipp", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amy", "MCgG", "LIM WM")
frontorb <- c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "FRO WM")
frontdors <- c("PrG", "MSFG", "SMC", "MFG", "SFG")
temporal <- c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG", "TMP WM")
parietal <- c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG", "PAR WM")
occipital <- c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG", "OCC WM")
cerebellum <- c("Vermis 1-5", "Vermis 6-7", "Vermis 8-10", "Cer Ext")
for(q in 1:6){
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

	sum_df <- data.frame(matrix(NA, nrow=length(colstouse), ncol=6))
	colnames(sum_df) <- c("Label", "Abbrev", "Lobe", "PFIT", "Female", "Male")

	for (i in 1:length(colstouse)) {
		sum_df[i, "Label"] <- colstouse[i]
		intname <- strsplit(colstouse[i], split="_")
		if (length(intname[[1]]) > 4) {
			tmpname <- ""
			for (k in 4:length(intname[[1]])) {
				if (k == 4) {
					tmpname <- paste0(tmpname, intname[[1]][k])
				} else {
					tmpname <- paste0(tmpname, "_", intname[[1]][k])
				}
			}
			intname <- tmpname
		} else {
			intname <- intname[[1]][4]
		}
		if (intname == "Cerebellar_Vermal_Lobules_I.V") { intname <- "Vermis 1-5"
		} else if (intname == "Cerebellar_Vermal_Lobules_VI.VII") { intname <- "Vermis 6-7"
		} else if (intname == "Cerebellar_Vermal_Lobules_VIII.X") { intname <- "Vermis 8-10"
		} else if (intname == "Accumbens_Area") { intname <- "Acc"
		} else if (intname == "Amygdala") { intname <- "Amy"
		} else if (intname == "Caudate") { intname <- "Cau"
		} else if (intname == "Cerebellum_Exterior") { intname <- "Cer Ext"
		} else if (intname == "Hippocampus") { intname <- "Hipp"
		} else if (intname == "Pallidum") { intname <- "Pall"
		} else if (intname == "Putamen") { intname <- "Put"
		} else if (intname == "Thalamus_Proper") { intname <- "Thal"
		} else if (intname == "Limbic_Lobe_WM") { intname <- "LIM WM"
		} else if (intname == "Insular_Lobe_WM") { intname <- "INS WM"
		} else if (intname == "Frontal_Lobe_WM") { intname <- "FRO WM"
		} else if (intname == "Parietal_Lobe_WM") { intname <- "PAR WM"
		} else if (intname == "Occipital_Lobe_WM") { intname <- "OCC WM"
		} else if (intname == "Temporal_Lobe_WM") { intname <- "TMP WM"
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
		}
	}
	
	sum_df$Lobe <- factor(sum_df$Lobe, levels = c("Basal Ganglia", "Limbic", "Frontal", "Temporal", "Parietal", "Occipital", "Cerebellum"))
	for (j in 1:nrow(sum_df)) {
		if (sum_df[j, "Abbrev"] %in% c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG", "ITG", "STG", "Cau", "MOG")) {
			sum_df[j, "PFIT"] <- "Yes"
		} else { sum_df[j, "PFIT"] <- "No" }
	}

	minscale <- round(min(c(sum_df$Female, sum_df$Male)), digits=1) 
	if (minscale > min(c(sum_df$Female, sum_df$Male))) { minscale <- minscale - .1 }
	maxscale <- round(max(c(sum_df$Female, sum_df$Male)), digits=1) 
	if (maxscale < max(c(sum_df$Female, sum_df$Male))) { maxscale <- maxscale + .1 }

	if (data.names[q] == "vol") {
		thistit <- "Volume: Coefficient for Complex Cognition"
		subtit <- "Vol ~ CompCogAcc + Age + Age^2 + Age^3 + QA (All Vars Scaled)"
	} else if (data.names[q] == "gmd") {
		thistit <- "Gray Matter Density: Coefficient for Complex Cognition"
		subtit <- "GMD ~ CompCogAcc + Age + Age^2 + Age^3 + QA (All Vars Scaled)"
	} else if (data.names[q] == "tr") {
		thistit <- "Mean Diffusivity: Coefficient for Complex Cognition"
		subtit <- "MD ~ CompCogAcc + Age + Age^2 + Age^3 + QA (All Vars Scaled)"
	} else if (data.names[q] == "cbf") {
		thistit <- "Cerebral Blood Flow: Coefficient for Complex Cognition"
		subtit <- "CBF ~ CompCogAcc + Age + Age^2 + Age^3 + QA (All Vars Scaled)"
	} else if (data.names[q] == "alff") {
		thistit <- "Amp of Low Freq Fluc: Coefficient for Complex Cognition"
		subtit <- "ALFF ~ CompCogAcc + Age + Age^2 + Age^3 + QA (All Vars Scaled)"
	} else if (data.names[q] == "reho") {
		thistit <- "Regional Homogeneity: Coefficient for Complex Cognition"
		subtit <- "ReHo ~ CompCogAcc + Age + Age^2 + Age^3 + QA (All Vars Scaled)"
	} 

	outPlot <- ggplot(sum_df, aes(x=Male, y=Female, color=Lobe)) + theme_minimal() + geom_point(aes(size=PFIT), alpha=.5) + 
		geom_text_repel(aes(label=Abbrev), nudge_x=c(-maxscale/8, maxscale/8), nudge_y=c(-maxscale/8, maxscale/8)) +
		scale_x_continuous(limits=c(minscale, maxscale), breaks=seq(minscale, maxscale, .05)) + 
		scale_y_continuous(limits=c(minscale, maxscale), breaks=seq(minscale, maxscale, .05)) +
		scale_color_manual(values=c("blue1", "slateblue1", "turquoise", "lightskyblue2", "plum2", "orange", "red")) +
		labs(title=thistit, subtitle=subtit) + xlab("Coefficient from Male Model") + ylab("Coefficient from Female Model") +
		geom_abline(intercept=0, slope=1, linetype="dotdash", alpha=.5) +
		theme(plot.title = element_text(size=16, face="bold"), plot.subtitle = element_text(size=12))
		
	assign(paste0(data.names[q], "_plot"), outPlot)
}


pdf('/home/butellyn/hiLo/plots/beta_F1_Exec_Comp_Cog_Accuracy.pdf', width=22, height=12)
grid.arrange(vol_plot, gmd_plot, tr_plot, cbf_plot, alff_plot, reho_plot, nrow=2, ncol=3)
dev.off()


