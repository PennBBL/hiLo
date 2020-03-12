### This script merges some of the densities
###
### Ellyn Butler
### March 12, 2020


library('ggplot2')
library('gridExtra')

justbrain_df <- read.csv("~/Documents/hiLo/data/permutationResults_half_NoReg.csv")
nested_df <- read.csv("~/Documents/hiLo/data/permutationResults_half_AgeQA_strongNull_OLSNoPenalizeAgeQA.csv")

justbrain_df$Permuted <- as.character(justbrain_df$Permuted)
nested_df$Null <- as.character(nested_df$Null)

names(justbrain_df)[names(justbrain_df) == "Permuted"] <- "Group"
names(nested_df)[names(nested_df) == "Null"] <- "Group"

justbrain_df[justbrain_df$Group == "No", "Group"] <- "Brain"
nested_df[nested_df$Group == "No", "Group"] <- "AgeBrain"
nested_df[nested_df$Group == "Yes", "Group"] <- "Age"

df <- rbind(justbrain_df, nested_df)
df <- df[df$Group != "Yes",]
rownames(df) <- 1:nrow(df)

# Summaries
justbrain_toPlot <- read.csv("~/Documents/hiLo/data/permutationSummary_half_NoReg.csv")
nested_toPlot <- read.csv("~/Documents/hiLo/data/permutationSummary_half_AgeQA_strongNull_OLSNoPenalizeAgeQA.csv")

justbrain_toPlot$Permuted <- as.character(justbrain_toPlot$Permuted)
nested_toPlot$Null <- as.character(nested_toPlot$Null)

names(justbrain_toPlot)[names(justbrain_toPlot) == "Permuted"] <- "Group"
names(nested_toPlot)[names(nested_toPlot) == "Null"] <- "Group"

justbrain_toPlot[justbrain_toPlot$Group == "No", "Group"] <- "Brain"
nested_toPlot[nested_toPlot$Group == "No", "Group"] <- "AgeBrain"
nested_toPlot[nested_toPlot$Group == "Yes", "Group"] <- "Age"

toPlot <- rbind(justbrain_toPlot, nested_toPlot)
toPlot <- toPlot[toPlot$Group != "Yes",]
rownames(toPlot) <- 1:nrow(toPlot)

results_df <- df
toPlotVals <- toPlot

results_df$Modality <- ordered(results_df$Modality, levels=c("Volume", "GMD", "MD",
  "CBF", "ALFF", "ReHo", "NBack", "IdEmo", "All"))

out.plot <- ggplot(results_df, aes(x=RSq, group=Group, fill=Group)) +
  geom_density(data=results_df[results_df$Group=='Age' & results_df$Sex=="Male",], fill="black") +
  geom_density(data=results_df[results_df$Group=='AgeBrain' & results_df$Sex=="Male",], fill="steelblue2", alpha=.5) +
  geom_density(data=results_df[results_df$Group=='Brain' & results_df$Sex=="Male",], fill="darkseagreen2", alpha=.5) +
  geom_density(data=results_df[results_df$Group=='Age' & results_df$Sex=="Female",], fill="black") +
  geom_density(data=results_df[results_df$Group=='AgeBrain' & results_df$Sex=="Female",], fill="violetred1", alpha=.5) +
  geom_density(data=results_df[results_df$Group=='Brain' & results_df$Sex=="Female",], fill="bisque", alpha=.5) +
  theme_linedraw() +
  facet_grid(Modality ~ Sex) +
  coord_cartesian(ylim=c(0,25),xlim=c(-.1,.6)) +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
  ylab("") +
  geom_vline(data = toPlotVals[toPlotVals$Group == "Age" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Group == "AgeBrain" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="steelblue2") +
  geom_vline(data = toPlotVals[toPlotVals$Group == "Brain" & toPlotVals$Sex == "Male", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="darkseagreen2") +
  geom_vline(data = toPlotVals[toPlotVals$Group == "Age" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Group == "AgeBrain" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="violetred1") +
  geom_vline(data = toPlotVals[toPlotVals$Group == "Brain" & toPlotVals$Sex == "Female", ],
    mapping = aes(xintercept = RSq), linetype = "dashed", color="bisque")



png(file="~/Documents/hiLo/plots/figure7_color_combo.png", height=160, width=120, units='mm', res=800)
out.plot
dev.off()
















    #
