## AFGR April 2018

## This script will be used to prepare the data, which will be fed to TPOT 
## The goal of this data is to classify persist vs resilient from T1 data only.
## This will be using our imaging summary metrics, clinical data, ers, and potentially prs
## FOr shits and gigs I'll even expand this to individal modalities

## Load library(s)
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('psych', 'mi', 'ForImp','methods', 'doParallel', 'pROC')

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
ers.data <- read.csv("/home/tymoore/Risk_Calculator_FULL.csv")
ers.data <- ers.data[,1:15]
## Quickly grab an index for bblid's to use
## Now we need to isolate to our labels of interest
tmp.index <- all.data[intersect(which(all.data$tpvalue==1),which(all.data$pncGrpPsychosisCl=="TD" | all.data$pncGrpPsychosisCl=="Emergent")),'bblid']
tmp.index2 <- all.data[intersect(which(all.data$tpvalue==1),which(all.data$pncGrpPsychosisCl=="TD" | all.data$pncGrpPsychosisCl=="Emergent")),'scanid']
output <- cbind(tmp.index, tmp.index2)
write.csv(output, "tdVsEmergSubj.csv", quote=F, row.names=F)
cog.data <- read.csv('/home/tymoore/n9498_cnb_zscores_all_frar_20161215.csv')
cog.data <- cog.data[,c(1:28,47,48)]
clin.data <- read.csv('/home/tymoore/GOASSESS_ITEMWISE.csv', na.strings=c("NA","NaN", " ", '.'))
val.index <- apply(clin.data[which(clin.data$PROBAND_BBLID %in% tmp.index),], 2, function(x) length(which(is.na(x)))/length(tmp.index))
to.use <- which(val.index<.1500001)
clin.data <- clin.data[,to.use]
clin.data <- clin.data[,-151]
clin.data.impute <-  read.csv('/home/tymoore/GOA112_imputed.csv')
clin.data.impute <- clin.data.impute[,-83]

## Now write out our clinical data for those that have the long labels
clin.data.impute.out <- clin.data.impute[which(clin.data.impute$bblid %in% tmp.index),]
ers.data <- ers.data[ers.data$bblid %in% tmp.index,]

# Now go through the same procedure for the cog.data
options(mc.cores=8)
mdf <- missing_data.frame(cog.data[,-1])
imputations <- mi(mdf, parallel=T, seed=16, n.chains=8)
output <- complete(imputations, 8)
output.1 <- output[[1]][,2:29]
output.2 <- output[[2]][,2:29]
output.3 <- output[[3]][,2:29]
output.4 <- output[[4]][,2:29]
output.5 <- output[[5]][,2:29]
output.6 <- output[[6]][,2:29]
output.7 <- output[[7]][,2:29]
output.8 <- output[[8]][,2:29]
output <- (output.1 + output.2 + output.3 + output.4 + output.5 + output.6 + output.7 + output.8)/8
cog.data[,3:30] <- output

## Now we need to isolate to our labels of interest
all.data.tu <- all.data[intersect(which(all.data$tpvalue==1),which(all.data$pncGrpPsychosisCl=="TD" | all.data$pncGrpPsychosisCl=="Emergent")),]
## Now prepare the demographic data
demo.data <- all.data.tu[,c('bblid', 'sex', 'race', 'envSES', 'scanageMonths')]
cog.data.out <- cog.data[cog.data$bblid %in% tmp.index,-2]
clin.data.impute.out <- clin.data.impute[clin.data.impute$bblid %in% tmp.index,]

## Now age and sex reregress the pertienent vars
pert.vars <- c(217:228, 685:696, 815:826, 1205:1216, 1535:1546, 1559:1570, 1583:1630)
all.data.tu[,pert.vars] <- apply(all.data.tu[,pert.vars], 2, function(x) regressOutAge(x, all.data.tu$scanageMonths, all.data.tu$sex))
age.sex.bblid <- all.data.tu[,c(1,1649,1645)]

# Now combine sex and age to our clinical and cog data and then age regress these variables
cog.data.out <- merge(age.sex.bblid, cog.data.out)
cog.data.out[,4:31] <- apply(cog.data.out[,4:31], 2, function(x) regressOutAge(x, cog.data.out$scanageMonths, cog.data.out$sex))

# Now age regress our clinical data 
clin.data.impute.out <- merge(age.sex.bblid, clin.data.impute.out)
clin.data.impute.out[,4:114] <- apply(clin.data.impute.out[,4:114], 2, function(x) regressOutAge(x, clin.data.impute.out$scanageMonths, clin.data.impute.out$sex))

# Now combine these two to create a data set with age regressed cog data 
# and nonAR clinical data
cog.and.clin <- merge(age.sex.bblid, cog.data.out)
cog.and.clin <- merge(cog.and.clin, demo.data)
cog.and.clin <- merge(cog.and.clin, clin.data.impute.out)

## Now isolate our variables of interest
vars.of.interest <- c(217:228, 685:696, 815:826, 1205:1216, 1535:1546, 1559:1570, 1583:1630)
toWrite <- all.data.tu[,vars.of.interest]
toWrite$y <- 0
toWrite$y[which(all.data.tu$pncGrpPsychosisCl=='Emergent')] <- 1
toWrite <- toWrite[complete.cases(toWrite),]
write.csv(toWrite, "forTpotImg.csv", quote=F, row.names=F)

# Now write the cog data
tmp.index <- merge(cog.data.out, all.data.tu, by='bblid')
cog.data.out$y <- 0
cog.data.out$y[which(tmp.index$pncGrpPsychosisCl=='Emergent')] <- 1
write.csv(cog.data.out, "forTpotCogPsych.csv", quote=F, row.names=F)

# Same thing for clinical
tmp.index <- merge(clin.data.impute.out, all.data.tu, by='bblid')
clin.data.impute.out$y <- 0
clin.data.impute.out$y[which(tmp.index$pncGrpPsychosisCl=='Emergent')] <- 1
write.csv(clin.data.impute.out, "forTpotClinPsych.csv", quote=F, row.names=F)

## Now do ers data
tmp.index <- merge(ers.data, all.data.tu, by='bblid')
ers.data$y <- 0
ers.data$y[which(tmp.index$pncGrpPsychosisCl=='Emergent')] <- 1
write.csv(ers.data, "forTpotErsPsych.csv", quote=F, row.names=F)

# Et cetra
tmp.index <- merge(demo.data, all.data.tu, by='bblid')
demo.data$y <- 0
demo.data$y[which(tmp.index$pncGrpPsychosisCl=='Emergent')] <- 1
write.csv(demo.data, "demoData.csv", quote=F, row.names=F)

tmp.index <- merge(cog.and.clin, all.data.tu, by='bblid')
cog.and.clin$y <- 0
cog.and.clin$y[which(tmp.index$pncGrpPsychosisCl=='Emergent')] <- 1
write.csv(cog.and.clin, "cogClinDemo.csv", quote=F, row.names=F)

## Now do imaging and cog and clin factor scores
vars.of.interest <- c(217:228, 685:696, 815:826, 1205:1216, 1535:1546, 1559:1570, 1583:1643, 1710:1712, 1651)
toWrite <- all.data.tu[,vars.of.interest]
toWrite$y <- 0
toWrite$y[which(all.data.tu$pncGrpPsychosisCl=='Emergent')] <- 1
toWrite <- toWrite[complete.cases(toWrite),]
write.csv(toWrite, "forTpotAll.csv", quote=F, row.names=F)
q()

## Here is the tpot call I will use on my local machine
## tpot forTpotPsychosis.csv -is , -target y -mode classification -scoring roc_auc -v 2 -cf forTpotPsych/

## Now I want to explore varibale selection based on t statistics
all.data.tu$y <- 0
all.data.tu$y[which(all.data.tu$pncGrpPsychosisCl=='Resilient')] <- 1
img.names <- c(grep('_jlf', names(all.data.tu)), grep('jhutract', names(all.data.tu)))
imgVals <- apply(all.data.tu[,img.names], 2, function(x) auc(roc(all.data.tu$y~x)))
index <- which(abs(imgVals)>.55)
img.index <- img.names[index]
## Now do cog and clinical scores
cog.names <- c(1631:1643, 1703:1711)
cogVals <- apply(all.data.tu[,cog.names], 2, function(x) auc(roc(all.data.tu$y~x)))
index <- which(abs(cogVals)>.55)
cog.index <- cog.names[index]

# Now add the envrinoment score
out.index <- c(img.index, cog.index, 1651, 1730)
toWrite <- all.data.tu[,out.index]

# Now explore imputation
toImpute <- toWrite[,-c(251)]
toImpute <- missing_data.frame(toImpute)
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

