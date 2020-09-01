### This script merges some of the densities
###
### Ellyn Butler
### March 12, 2020 - April 8, 2020


library('ggplot2')
library('gridExtra')
library('dplyr')

results_df <- read.csv('~/Documents/hiLo/data/r2results/results_ridgeRSq_combined.csv')
toPlotVals <- read.csv('~/Documents/hiLo/data/r2results/summary_ridgeRSq_combined.csv')
toPlotVals <- toPlotVals[,c("Modality", "Sex", "Group", "RSq")]

toPlotVals2 <- data.frame(matrix(NA, nrow=18, ncol=4))
names(toPlotVals2) <- names(toPlotVals)
toPlotVals2$Modality <- c('Volume', 'Volume', 'GMD', 'GMD', 'MD', 'MD', 'CBF',
  'CBF', 'ALFF', 'ALFF', 'ReHo', 'ReHo', 'NBack', 'NBack', 'IdEmo', 'IdEmo', 'All', 'All')
toPlotVals2$Sex <- rep(c("Female", "Male"), 9)
toPlotVals2$Group <- 'AgeBrain Minus Age'
toPlotVals2$RSq <- toPlotVals[toPlotVals$Group == "AgeBrain", "RSq"] - toPlotVals[toPlotVals$Group == "Age", "RSq"]

toPlotVals <- rbind(toPlotVals, toPlotVals2)
toPlotVals <- arrange(toPlotVals, Group, Sex)

write.csv(toPlotVals, file='~/Documents/hiLo/data/r2results/summary_R2.csv', row.names=FALSE)



#AgeBrain Minus Age
diff_df <- data.frame(matrix(NA, nrow=18000, ncol=5))
names(diff_df) <- names(results_df)
diff_df$Modality <- c(rep("Volume", 2000), rep("GMD", 2000),
  rep("MD", 2000), rep("CBF", 2000), rep("ALFF", 2000), rep("ReHo", 2000),
  rep("NBack", 2000), rep("IdEmo", 2000), rep("All", 2000))
diff_df$Sex <- rep(c("Female", "Male"), 9000)
diff_df$Group <- rep("AgeBrain Minus Age", 18000)
diff_df$Run <- rep(1:1000, 18)
diff_df$RSq <- results_df[results_df$Group == "AgeBrain", "RSq"] - results_df[results_df$Group == "Age", "RSq"]

results_df <- rbind(results_df, diff_df)

results_df$Modality <- ordered(results_df$Modality, levels=c('Volume', 'GMD', 'MD',
  'CBF', 'ALFF', 'ReHo', 'NBack', 'IdEmo', 'All'))


################ Plot ################

colsfill_manu <- c('Brain'='black', 'Age'='gray62', 'AgeBrain'='white')
ann_text <- data.frame(Sex=factor(c("Female", "Male", "Female", "Female", "Female")),
  Modality=factor(c(rep("Volume", 2), "GMD", "NBack", "All"), levels=unique(results_df$Modality)),
  lab = "*", RSq=0)
out.plot_brain_age_agebrain <- ggplot(results_df, aes(x=RSq)) + theme_linedraw() +
  geom_density(aes(fill='Brain', group=1), data=results_df[results_df$Group=='Brain' & results_df$Sex=='Male',]) +
  geom_density(aes(fill='Age', group=1), data=results_df[results_df$Group=='Age' & results_df$Sex=='Male',], alpha=.7) +
  geom_density(aes(fill='AgeBrain', group=1), data=results_df[results_df$Group=='AgeBrain' & results_df$Sex=='Male',], alpha=.7) +
  geom_density(aes(fill='Brain', group=1), data=results_df[results_df$Group=='Brain' & results_df$Sex=='Female',]) +
  geom_density(aes(fill='Age', group=1), data=results_df[results_df$Group=='Age' & results_df$Sex=='Female',], alpha=.7) +
  geom_density(aes(fill='AgeBrain', group=1), data=results_df[results_df$Group=='AgeBrain' & results_df$Sex=='Female',], alpha=.7) +
  scale_fill_manual(values=colsfill_manu, breaks=c("Brain", "Age", "AgeBrain"),
    name="Variables", labels = c("Brain", "Age", "Age + Brain")) +
  scale_colour_manual(values=c('black', 'black', 'black', 'black')) +
  facet_grid(Modality ~ Sex) +
  coord_cartesian(ylim=c(0,25),xlim=c(-.1,.6)) +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position='bottom') +
  ylab('') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'Age' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'AgeBrain' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'Brain' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'Age' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'AgeBrain' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'Brain' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_text(data=ann_text, y=12, label="*", size=15)

results_df2 <- results_df[results_df$Group %in% c("QA", "QABrain", "AgeQA", "AgeQABrain"),]
results_df2$Group <- ordered(results_df2$Group, levels=c("QA", "QABrain", "AgeQA", "AgeQABrain"))
colsfill <- c("QA"='black', "QABrain"='aquamarine4', "AgeQA"='aquamarine3', "AgeQABrain"='aquamarine')
out.plot_qa_qabrain_ageqa_ageqabrain <- ggplot(results_df2, aes(x=RSq)) +
  geom_density(aes(fill='QA', group=1), data=results_df2[results_df2$Group=='QA' & results_df2$Sex=='Male',]) +
  geom_density(aes(fill='QABrain', group=1), alpha=.6, data=results_df2[results_df2$Group=='QABrain' & results_df2$Sex=='Male',]) +
  geom_density(aes(fill='AgeQA', group=1), alpha=.6, data=results_df2[results_df2$Group=='AgeQA' & results_df2$Sex=='Male',]) +
  geom_density(aes(fill='AgeQABrain', group=1), alpha=.6, data=results_df2[results_df2$Group=='AgeQABrain' & results_df2$Sex=='Male',]) +
  geom_density(aes(fill='QA', group=1), data=results_df2[results_df2$Group=='QA' & results_df2$Sex=='Female',]) +
  geom_density(aes(fill='QABrain', group=1), alpha=.6, data=results_df2[results_df2$Group=='QABrain' & results_df2$Sex=='Female',]) +
  geom_density(aes(fill='AgeQA', group=1), alpha=.6, data=results_df2[results_df2$Group=='AgeQA' & results_df2$Sex=='Female',]) +
  geom_density(aes(fill='AgeQABrain', group=1), alpha=.6, data=results_df2[results_df2$Group=='AgeQABrain' & results_df2$Sex=='Female',]) +
  scale_fill_manual(values=colsfill, breaks=c("QA", "QABrain", "AgeQA", "AgeQABrain"), name="Variables") +
  scale_colour_manual(values=c('black', 'black', 'black', 'black')) +
  theme_linedraw() +
  facet_grid(Modality ~ Sex) +
  coord_cartesian(ylim=c(0,25),xlim=c(-.1,.6)) +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position='bottom') +
  ylab('') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'QA' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'QABrain' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'AgeQA' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'AgeQABrain' & toPlotVals$Sex == 'Male', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'QA' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'QABrain' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'AgeQA' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black') +
  geom_vline(data = toPlotVals[toPlotVals$Group == 'AgeQABrain' & toPlotVals$Sex == 'Female', ],
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black')


tiff(file='~/Documents/hiLo/plots/figure7.tiff', height=180, width=120, units='mm', res=300)
out.plot_brain_age_agebrain
dev.off()

png(file='~/Documents/hiLo/plots/figure7_supplement.png', height=180, width=120, units='mm', res=800)
out.plot_qa_qabrain_ageqa_ageqabrain
dev.off()










    #
