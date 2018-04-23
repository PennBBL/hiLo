# Load library(s)
install_load('ggplot2','reshape2')

# Load the data
all.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')

## Now isolate males
male.vals <- subset(all.vals, sex==1)

# Now make a rank for each subject
rank.vals.male <- apply(male.vals[,grep('_jlf_', names(male.vals))], 2, rank)

## Now see the variance across each of these subjects
toPlot <- melt(rank.vals.male)
viol.plot.1 <- ggplot(toPlot, aes(x=factor(Var1), y=value)) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23) +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  theme(axis.text.x=element_text(angle=90))
pdf('testPlot.pdf', height=12, width=30)
viol.plot.1
dev.off()
