# AFGR December 2017

# This script is going to be used to compare differenes in ranks across our different factors.
# This will be limited to our imaging cohort

# Load library(s)
install_load('ggplot2')

# Load the data
all.vals <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')


# Now isolate the factor values
fac.vals.male <- all.vals[which(all.vals$sex==1),4:16]
fac.vals.female <- all.vals[which(all.vals$sex==2),4:16]
fac.vals.male.rank <- apply(fac.vals.male, 2, rank)
fac.vals.female.rank <- apply(fac.vals.female, 2, rank)

# Now loop through each column and plot it vs F1
colVals <- c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)
pdf('maleRankCompare.pdf')
for(i in colVals){
  # First I need to find the difference between F1 and our other metric of interest
  diff.val <- fac.vals.male.rank[,4] - fac.vals.male.rank[,i]
  # Now find the standard deviation of the difference value
  diff.sd <- sd(diff.val)
  # Now find the corellation
  nameval <- colnames(fac.vals.male.rank)[i]
  corVal <- paste("r = ", round(cor(fac.vals.male.rank[,4], fac.vals.male.rank[,i]), digits=2))
  # Now create a plot
  tmpDat <- as.data.frame(cbind(fac.vals.male.rank[,4], fac.vals.male.rank[,i]))
  outPlot <- ggplot(tmpDat, aes(x=V1, y=V2)) + geom_point() + geom_smooth(method=lm) + 
    xlab("F1_Exec_Comp_Cog_Accuracy Rank") + ylab(paste(nameval, "Rank", sep=' ')) + 
    geom_abline(intercept=0, slope=1) + 
    geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))
  print(outPlot)
    
}
dev.off()

pdf('femaleRankCompare.pdf')
for(i in colVals){
  # First I need to find the difference between F1 and our other metric of interest
  diff.val <- fac.vals.female.rank[,4] - fac.vals.female.rank[,i]
  # Now find the standard deviation of the difference value
  diff.sd <- sd(diff.val)
  # Now find the corellation
  nameval <- colnames(fac.vals.female.rank)[i]
  corVal <- paste("r = ", round(cor(fac.vals.female.rank[,4], fac.vals.female.rank[,i]), digits=2))
  # Now create a plot
  tmpDat <- as.data.frame(cbind(fac.vals.male.rank[,4], fac.vals.male.rank[,i]))
  outPlot <- ggplot(tmpDat, aes(x=V1, y=V2)) + geom_point() + geom_smooth(method=lm) + 
    xlab("F1_Exec_Comp_Cog_Accuracy Rank") + ylab(paste(nameval, "Rank", sep=' ')) + 
    geom_abline(intercept=0, slope=1) + 
    geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))
  print(outPlot)
    
}
dev.off()

# Now create cor matrices of the factor scores
#tmp <- melt(cor(fac.vals.male))

