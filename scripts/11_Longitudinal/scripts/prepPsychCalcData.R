## AFGR April 2018

## This script will be used to prepare the data, which will be fed to TPOT 
## The goal of this data is to classify persist vs resilient from T1 data only.
## This will be using our imaging summary metrics, clinical data, ers, and potentially prs
## FOr shits and gigs I'll even expand this to individal modalities

## Load library(s)
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('psych', 'mi')

## Declare any functions
regressOutAge <- function(valuesToBeRegressed, ageColumn, sexColumn){
    # First declare an output column
    newValues <- rep(NA, length(valuesToBeRegressed))
    index <- which(complete.cases(valuesToBeRegressed))
    age <- scale(ageColumn)[,1]
    ageSqu <- scale(ageColumn)[,1]^2
    ageCub <- scale(ageColumn)[,1]^3
    newValues[index] <- lm(valuesToBeRegressed ~ age + ageSqu + ageCub + sexColumn)$residuals
    return(newValues)
}

## Load all of our data
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-04-22.csv')

## Now we need to isolate to our labels of interest
all.data.tu <- all.data[intersect(which(all.data$tpvalue==1),which(all.data$pncGrpPsychosisCl=="Persister" | all.data$pncGrpPsychosisCl=="Resilient")),]

## Now age and sex reregress the pertienent vars
pert.vars <- c(1523:1534, 454)
all.data.tu[,pert.vars] <- apply(all.data.tu[,pert.vars], 2, function(x) regressOutAge(x, all.data.tu$scanageMonths, all.data.tu$sex))

## Now isolate our variables of interest
vars.of.interest <- c(217:228, 685:696, 815:828, 1205:1216, 1535:1630, 1634:1643, 1651, 1710:1712)
toWrite <- all.data.tu[,vars.of.interest]
toWrite$y <- 0
toWrite$y[which(all.data.tu$pncGrpPsychosisCl=='Resilient')] <- 1
write.csv(toWrite, "forTpotPsychosis.csv", quote=F, row.names=F)

## Here is the tpot call I will use on my local machine
## tpot forTpotPsychosis.csv -is , -target y -mode classification -scoring roc_auc -v 2 -cf forTpotPsych/

## Now I want to explore varibale selection based on t statistics
all.data.tu$y <- 0
all.data.tu$y[which(all.data.tu$pncGrpPsychosisCl=='Resilient')] <- 1
img.names <- c(grep('_jlf', names(all.data.tu)), grep('jhutract', names(all.data.tu)))
imgVals <- apply(all.data.tu[,img.names], 2, function(x) t.test(x~ all.data.tu$y)$statistic)
index <- which(abs(imgVals)>1.8)
img.index <- img.names[index]
## Now do cog and clinical scores
cog.names <- c(1631:1643, 1703:1711)
cogVals <- apply(all.data.tu[,cog.names], 2, function(x) t.test(x~ all.data.tu$y)$statistic)
index <- which(abs(cogVals)>1.7)
cog.index <- cog.names[index]

# Now add the envrinoment score
out.index <- c(img.index, cog.index, 1651, 1730)
toWrite <- all.data.tu[,out.index]

# Now explore imputation
toImpute <- toWrite[,-c(93)]
toImpute <- mi(toImpute, parallel=T, seed=16)
output <- complete(toImpute, 4)
output.1 <- output[[1]][,1:92]
output.2 <- output[[2]][,1:92]
output.3 <- output[[3]][,1:92]
output.4 <- output[[4]][,1:92]
output <- (output.1 + output.2 + output.3 + output.4)/4
toWrite[,1:92] <- output
# now write it
write.csv(toWrite, "forTpotPsychosisVarSelect.csv", quote=F, row.names=F)

## Here is the tpot call I will use on my local machine
## tpot forTpotPsychosisVarSelect.csv -is , -target y -mode classification -scoring roc_auc -v 2 -cf forTpotPsych/
