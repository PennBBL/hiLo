### This script merges some of the densities
###
### Ellyn Butler
### March 12, 2020 - March 16, 2020


library('ggplot2')
library('gridExtra')

################ Results ################
brain_df <- read.csv('~/Documents/hiLo/data/r2results/permutationResults_half_NoReg.csv')
ageqa_ageqabrain_df <- read.csv('~/Documents/hiLo/data/r2results/permutationResults_half_AgeQA_strongNull_OLSNoPenalizeAgeQA.csv')
age_agebrain_df <- read.csv('~/Documents/hiLo/data/r2results/permutationResults_half_Age_strongNull_OLSNoPenalizeAge.csv')
qa_qabrain_df <- read.csv('~/Documents/hiLo/data/r2results/permutationResults_half_QA_strongNull_OLSNoPenalizeQA.csv')

brain_df$Permuted <- as.character(brain_df$Permuted)
ageqa_ageqabrain_df$Null <- as.character(ageqa_ageqabrain_df$Null)
age_agebrain_df$Null <- as.character(age_agebrain_df$Null)
qa_qabrain_df$Null <- as.character(qa_qabrain_df$Null)

names(brain_df)[names(brain_df) == 'Permuted'] <- 'Group'
names(ageqa_ageqabrain_df)[names(ageqa_ageqabrain_df) == 'Null'] <- 'Group'
names(age_agebrain_df)[names(age_agebrain_df) == 'Null'] <- 'Group'
names(qa_qabrain_df)[names(qa_qabrain_df) == 'Null'] <- 'Group'

brain_df[brain_df$Group == 'No', 'Group'] <- 'Brain'
ageqa_ageqabrain_df[ageqa_ageqabrain_df$Group == 'No', 'Group'] <- 'AgeQABrain'
ageqa_ageqabrain_df[ageqa_ageqabrain_df$Group == 'Yes', 'Group'] <- 'AgeQA'

age_agebrain_df[age_agebrain_df$Group == 'No', 'Group'] <- 'AgeBrain'
age_agebrain_df[age_agebrain_df$Group == 'Yes', 'Group'] <- 'Age'
qa_qabrain_df[qa_qabrain_df$Group == 'No', 'Group'] <- 'QABrain'
qa_qabrain_df[qa_qabrain_df$Group == 'Yes', 'Group'] <- 'QA'

final_df <- rbind(brain_df, ageqa_ageqabrain_df)
final_df <- rbind(final_df, age_agebrain_df)
final_df <- rbind(final_df, qa_qabrain_df)
final_df <- final_df[final_df$Group != 'Yes',]
row.names(final_df) <- 1:nrow(final_df)

################ Summaries ################
brain_toPlot <- read.csv('~/Documents/hiLo/data/r2results/permutationSummary_half_NoReg.csv')
ageqa_ageqabrain_toPlot <- read.csv('~/Documents/hiLo/data/r2results/permutationSummary_half_AgeQA_strongNull_OLSNoPenalizeAgeQA.csv')
age_agebrain_toPlot <- read.csv('~/Documents/hiLo/data/r2results/permutationSummary_half_Age_strongNull_OLSNoPenalizeAge.csv')
qa_qabrain_toPlot <- read.csv('~/Documents/hiLo/data/r2results/permutationSummary_half_QA_strongNull_OLSNoPenalizeQA.csv')

brain_toPlot$Permuted <- as.character(brain_toPlot$Permuted)
ageqa_ageqabrain_toPlot$Null <- as.character(ageqa_ageqabrain_toPlot$Null)
age_agebrain_toPlot$Null <- as.character(age_agebrain_toPlot$Null)
qa_qabrain_toPlot$Null <- as.character(qa_qabrain_toPlot$Null)

names(brain_toPlot)[names(brain_toPlot) == 'Permuted'] <- 'Group'
names(ageqa_ageqabrain_toPlot)[names(ageqa_ageqabrain_toPlot) == 'Null'] <- 'Group'
names(age_agebrain_toPlot)[names(age_agebrain_toPlot) == 'Null'] <- 'Group'
names(qa_qabrain_toPlot)[names(qa_qabrain_toPlot) == 'Null'] <- 'Group'

brain_toPlot[brain_toPlot$Group == 'No', 'Group'] <- 'Brain'
ageqa_ageqabrain_toPlot[ageqa_ageqabrain_toPlot$Group == 'No', 'Group'] <- 'AgeQABrain'
ageqa_ageqabrain_toPlot[ageqa_ageqabrain_toPlot$Group == 'Yes', 'Group'] <- 'AgeQA'
age_agebrain_toPlot[age_agebrain_toPlot$Group == 'No', 'Group'] <- 'AgeBrain'
age_agebrain_toPlot[age_agebrain_toPlot$Group == 'Yes', 'Group'] <- 'Age'
qa_qabrain_toPlot[qa_qabrain_toPlot$Group == 'No', 'Group'] <- 'QABrain'
qa_qabrain_toPlot[qa_qabrain_toPlot$Group == 'Yes', 'Group'] <- 'QA'

toPlot <- rbind(brain_toPlot, ageqa_ageqabrain_toPlot)
toPlot <- rbind(toPlot, age_agebrain_toPlot)
toPlot <- rbind(toPlot, qa_qabrain_toPlot)
toPlot <- toPlot[toPlot$Group != 'Yes',]
rownames(toPlot) <- 1:nrow(toPlot)

results_df <- final_df
toPlotVals <- toPlot

write.csv(toPlotVals[,c("Modality", "Sex", "Group", "RSq")], file='~/Documents/hiLo/data/r2results/summary_R2.csv', row.names=FALSE)

results_df$Modality <- ordered(results_df$Modality, levels=c('Volume', 'GMD', 'MD',
  'CBF', 'ALFF', 'ReHo', 'NBack', 'IdEmo', 'All'))

################ Plot ################
# TO DO: Add legends

#results_df1 <- results_df[results_df$Group %in% c("Brain", "Age", "AgeBrain"),]
#'black', 'steelblue2', 'darkseagreen2', 'black', 'violetred1', 'bisque'
colsfill_manu <- c('Brain'='black', 'Age'='gray62', 'AgeBrain'='white')
out.plot_brain_age_agebrain <- ggplot(results_df, aes(x=RSq)) +
  geom_density(aes(fill='Brain', group=1), data=results_df[results_df$Group=='Brain' & results_df$Sex=='Male',]) +
  geom_density(aes(fill='Age', group=1), data=results_df[results_df$Group=='Age' & results_df$Sex=='Male',], alpha=.7) +
  geom_density(aes(fill='AgeBrain', group=1), data=results_df[results_df$Group=='AgeBrain' & results_df$Sex=='Male',], alpha=.7) +
  geom_density(aes(fill='Brain', group=1), data=results_df[results_df$Group=='Brain' & results_df$Sex=='Female',]) +
  geom_density(aes(fill='Age', group=1), data=results_df[results_df$Group=='Age' & results_df$Sex=='Female',], alpha=.7) +
  geom_density(aes(fill='AgeBrain', group=1), data=results_df[results_df$Group=='AgeBrain' & results_df$Sex=='Female',], alpha=.7) +
  scale_fill_manual(values=colsfill_manu, breaks=c("Brain", "Age", "AgeBrain"), name="Variables") +
  scale_colour_manual(values=c('black', 'black', 'black', 'black')) +
  theme_linedraw() +
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
    mapping = aes(xintercept = RSq), linetype = 'dashed', color='black')

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


png(file='~/Documents/hiLo/plots/figure7.png', height=180, width=120, units='mm', res=800)
out.plot_brain_age_agebrain
dev.off()

png(file='~/Documents/hiLo/plots/figure7_supplement.png', height=180, width=120, units='mm', res=800)
out.plot_qa_qabrain_ageqa_ageqabrain
dev.off()










    #
