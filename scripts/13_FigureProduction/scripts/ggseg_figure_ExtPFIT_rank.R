### This script creates (hopefully) the final ggseg figure
###
### Ellyn Butler
### January 29, 2020 - June 1, 2020

library('ggseg')
library('ggplot2')
library('dplyr')
library('ggpubr')
library('R.utils')

load("~/Documents/ggsegExtra/data/micCort.rda")
rank_df <- read.csv("~/Documents/hiLo/data/lm_perf-effect_fdr_wES_052020.csv")
rank_df <- rank_df[,c("ROI", "mnrnk")]
rank_df$ROI <- recode(rank_df$ROI, "Hippocampus"="Hipp", "Amygdala"="Amy",
  "Pallidum"="Pall", "Putamen"="Put", "Accumbens_Area"="NA", "Caudate"="Cau",
  "Thalamus_Proper"="Thal")
rank_df <- rank_df[complete.cases(rank_df$mnrnk),]
for (i in 1:nrow(rank_df)) {
  if (rank_df[i, "mnrnk"] <= 10) { rank_df[i, "RankGroup"] <- 10
  } else if (rank_df[i, "mnrnk"] <= 20) { rank_df[i, "RankGroup"] <- 20
  } else if (rank_df[i, "mnrnk"] <= 30) { rank_df[i, "RankGroup"] <- 30
  } else if (rank_df[i, "mnrnk"] <= 40) { rank_df[i, "RankGroup"] <- 40
  } else if (rank_df[i, "mnrnk"] <= 50) { rank_df[i, "RankGroup"] <- 50
  } else if (rank_df[i, "mnrnk"] <= 60) { rank_df[i, "RankGroup"] <- 60 }
}

names(rank_df) <- c("Abbrev", "mnrnk", "RankGroup")
rank_df$Abbrev <- as.character(rank_df$Abbrev)
rank_df$Abbrev <- recode(rank_df$Abbrev, "PIns"="Pins")

base_atlas = micCort %>%
  select(area, hemi) %>%
  distinct() %>%
  na.omit() %>%
    mutate(PFIT=NA, Important=NA, Top=NA)

PFITregions <- c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG",
  "ITG", "STG", "Cau", "MOG")
ExtPFITregions <- c("OrIFG", "POrG", "Hipp", "OFuG", "AnG", "Thal", "TMP",
  "MSFG", "SMG", "NA")

base_atlas$PFIT <- as.character(base_atlas$PFIT)
base_atlas$Important <- as.character(base_atlas$Important)
base_atlas$Top <- as.numeric(base_atlas$Top)
for (i in 1:nrow(base_atlas)) {
  area <- as.character(base_atlas[i, "area"])
  if (area %in% PFITregions) {
    base_atlas[i, "PFIT"] <- "Yes"
    base_atlas[i, "Important"] <- "Yes"
  } else if (area %in% ExtPFITregions) {
    base_atlas[i, "PFIT"] <- "No"
    base_atlas[i, "Important"] <- "Yes"
  } else {
    base_atlas[i, "PFIT"] <- "No"
    base_atlas[i, "Important"] <- "No"
  }
  base_atlas[i, "Top"] <- rank_df[rank_df$Abbrev == area, "RankGroup"]
}

final_data <- base_atlas[base_atlas$hemi == "left",]
final_data$Top <- as.character(final_data$Top)
final_data$Top2 <- paste0(final_data$Important, final_data$PFIT, final_data$Top)
final_data$Top2 <- recode(final_data$Top2, "YesYes10"="PFIT 10", "YesYes40"="PFIT 40",
  "YesNo10"="ExtPFIT 10", "NoNo40"="Other", "NoNo50"="Other", "NoNo30"="Other",
  "YesYes20"="PFIT 20", "YesNo20"="ExtPFIT 20", "NoNo60"="Other", "YesYes50"="PFIT 50")
final_data$Top2 <- ordered(final_data$Top2, c("PFIT 10", "PFIT 20", "PFIT 30",
  "PFIT 40", "PFIT 50", "ExtPFIT 10", "ExtPFIT 20", "Other"))

p <- ggseg(final_data, atlas="micCort", mapping=aes(fill=Top2), hemisphere="left", size=.1,
  colour="black") +
  scale_fill_manual(values=c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
    "aliceblue", "#67001f", "#b2182b", "cornsilk"), drop=FALSE) +
  #labs(fill="Ranked Top #") +
  theme(text=element_text(size=14,  family="Arial"), axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position="none")


###############################################################################

aseg_data <- aseg

rank_df2 <- rank_df
rank_df2$Abbrev <- recode(rank_df2$Abbrev, "Hipp"="hippocampus", "Thal"="thalamus proper",
  "Put"="putamen", "Amy"="amygdala", "Pall"="pallidum", "Cau"="caudate")

aseg_data$PFIT <- as.character(aseg_data$PFIT)
aseg_data$Important <- as.character(aseg_data$Important)
aseg_data$Top <- as.numeric(aseg_data$Top)

PFITregions <- c("caudate")
ExtPFITregions <- c("hippocampus", "thalamus proper")

for (i in 1:nrow(aseg_data)) {
  area <- as.character(aseg_data[i, "area"])
  if (!is.na(area) & area %in% unique(rank_df2$Abbrev)) {
    if (area %in% PFITregions) {
      aseg_data[i, "PFIT"] <- "Yes"
      aseg_data[i, "Important"] <- "Yes"
    } else if (area %in% ExtPFITregions) {
      aseg_data[i, "PFIT"] <- "No"
      aseg_data[i, "Important"] <- "Yes"
    } else {
      aseg_data[i, "PFIT"] <- "No"
      aseg_data[i, "Important"] <- "No"
    }
    aseg_data[i, "Top"] <- rank_df2[rank_df2$Abbrev == area, "RankGroup"]
  }
}

aseg_data$Top <- as.character(aseg_data$Top)
aseg_data$Top2 <- paste0(aseg_data$Important, aseg_data$PFIT, aseg_data$Top)
aseg_data$Top2 <- recode(aseg_data$Top2, "YesYes10"="PFIT 10", "YesYes40"="PFIT 40",
  "YesNo10"="ExtPFIT 10", "NoNo40"="Other", "NoNo50"="Other", "NoNo30"="Other",
  "YesYes20"="PFIT 20", "YesNo20"="ExtPFIT 20", "NoNo60"="Other",
  "YesYes50"="PFIT 50")
aseg_data$Top2 <- ordered(aseg_data$Top2, c("PFIT 10", "PFIT 20", "PFIT 30",
  "PFIT 40", "PFIT 50", "ExtPFIT 10", "ExtPFIT 20", "Other"))
aseg_data <- aseg_data[!is.na(aseg_data$Top2),]


p_subcort <- ggseg(aseg_data, atlas="aseg", hemisphere=c("left", "right"),
  mapping=aes(fill=Top2), size=.1, colour="black", na.rm=TRUE) +
  scale_fill_manual(values=c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
    "aliceblue", "#67001f", "#b2182b", "cornsilk"), labels=c("PFIT 10", "PFIT 20",
    "PFIT 30", "PFIT 40", "PFIT 50", "ExtPFIT 10", "ExtPFIT 20", "Other"), drop=FALSE) +
  labs(fill="Ranked Top #") +
  theme(text=element_text(size=14,  family="Arial"), axis.title.x=element_blank(),
    axis.text.x=element_blank())









png(file="~/Documents/hiLo/plots/figure2_color_ExtPFIT_rank.png", units="mm", width=150, height=140, res=800)
ggarrange(p, p_subcort, nrow= 2, labels = c("A", "B"))
dev.off()
