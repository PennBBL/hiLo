### This script creates (hopefully) the final ggseg figure
###
### Ellyn Butler
### January 29, 2020 - January 31, 2020

library('ggseg')
library('ggplot2')
library('tidyverse')
library('R.utils')

load("/Users/butellyn/Documents/ggsegExtra/data/micCort.rda")

base_atlas = micCort %>%
  select(area,hemi) %>%
  distinct() %>%
  na.omit()

new_data_F = base_atlas %>%
  mutate(PFIT=NA, Count=NA)

new_data_M = base_atlas %>%
  mutate(PFIT=NA, Count=NA)

# Load effect sizes
i=1
for (eff in c("alff", "cbf", "gmd", "idemo", "nback", "reho", "tr", "vol")) {
  tmp_df <- read.csv(paste0("/Users/butellyn/Documents/hiLo/data/effsizes/", eff, ".csv"))
  if (i == 1) { this_df <- tmp_df
  } else if (i > 1) { this_df <- rbind(this_df, tmp_df) }
  i=i+1
}
this_df <- this_df[!(this_df$Lobe %in% c("White Matter", "Cerebellum")),]

F_df <- this_df[this_df$ageBin == "Adults" & this_df$Sex == "Female",]
rownames(F_df) <- 1:nrow(F_df)
M_df <- this_df[this_df$ageBin == "Adults" & this_df$Sex == "Male",]
rownames(M_df) <- 1:nrow(M_df)

PFITregions <- c("MFG", "IFG", "SFG", "SMC", "ACgG", "SPL", "PCu", "MTG", "PCgG", "ITG", "STG", "Cau", "MOG")
for (i in 1:nrow(new_data_F)) {
  # Adult Females
  area <- as.character(new_data_F[i, "area"])
  if (area %in% PFITregions) { pfitstat <- "PFIT" } else { pfitstat <- "Not PFIT" }
  new_data_F[i, "PFIT"] <- pfitstat
  new_data_F[i, "Count"] <- nrow(F_df[F_df$Abbrev == area & abs(F_df$EffectSize) >=.4,])

  # Adult Males
  new_data_M[i, "PFIT"] <- pfitstat
  new_data_M[i, "Count"] <- nrow(M_df[M_df$Abbrev == area & abs(M_df$EffectSize) >=.4,])
}

new_data_F$Gender <- "Female"
new_data_M$Gender <- "Male"
final_data <- rbind(new_data_F, new_data_M)
final_data <- final_data[final_data$hemi == "left",]
final_data$Count <- as.factor(final_data$Count)
final_data$Count2 <- paste(final_data$Count, final_data$PFIT)
final_data$Count2 <- factor(final_data$Count2, levels=c("0 Not PFIT", "1 Not PFIT", "2 Not PFIT",
  "3 Not PFIT", "4 Not PFIT", "5 Not PFIT", "6 Not PFIT", "0 PFIT", "1 PFIT", "2 PFIT",
  "3 PFIT", "4 PFIT", "5 PFIT", "6 PFIT"))

p <- ggseg(final_data, atlas="micCort", mapping=aes(fill=Count2), hemisphere="left", size=.1, colour="black") +
  scale_fill_manual(values=c("antiquewhite1", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f", "black",
    "white", "aliceblue", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"), drop=FALSE) +
  facet_wrap(~Gender, ncol=1) + labs(fill="# Modalities >.4") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank())


pdf(file="/Users/butellyn/Documents/hiLo/plots/brainFigure.pdf", width=7, height=5)
p
dev.off()

png(file="/Users/butellyn/Documents/hiLo/plots/figure2_color.png", units="mm", width=180, height=100, res=800)
p
dev.off()





#scale_fill_manual(values=c("white", "lavenderblush", "lavenderblush2", "pink1", "pink2", "pink3", "pink4",
#"white", "lightcyan1", "lightblue2", "cadetblue3", "steelblue2", "steelblue3", "darkslateblue"), drop=FALSE) +

#scale_fill_manual(values=c("white", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83", "#40004b", "black",
#"white", "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"), drop=FALSE) +
