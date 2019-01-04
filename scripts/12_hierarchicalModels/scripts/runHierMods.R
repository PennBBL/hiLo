install_load('psych','visreg','gtools','lme4','lmerTest','ggplot2','reshape','foreach','doParallel')

## Now here we need to source my avg l and r hemisphere values function
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")

# Declare any functions
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.33,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 'lo'
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 'me'
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 'hi'
  return(data)
}

#########################################################################
# Load and prep all data 
#########################################################################
x <- read.csv('/home/adrose/forRuben/data/tmp/n1601_imagingDataDump_2018-09-20.csv')
## Now run the script which prepares bilateral lobes, via Kosha's method
#source('./prepareLaterHiLoLobes.R')
#x <- merge(x, all.out)
# Now add year from CNB exlcusion
x <- x[which(abs(x$ageAtScan1 - x$ageAtCnb1)<=12),]
dim(x)
# Now add health excludes
health<-read.csv("/home/analysis/psycha1/pnc/n1601_health_20161214.csv")
x <- merge(health, x)
x <- x[which(x$incidentalFindingExclude==0),]
dim(x)
## First add age bins
x <- addAgeBin(x, x$ageAtScan1, 167, 215, 216)
## Now add perf bin
x <- returnPerfBin(x)
## Now ensure they are factors
x$perfBin <- factor(x$perfBin)
x$agein <- factor(x$ageBin)

#########################################################################
# Run global models
#########################################################################
global.val <- c("mprage_jlf_vol_TBV","mprage_jlf_vol_TBGM","mprage_jlf_vol_TBWM", "mprage_jlf_gmd_MeanGMD", "dti_jlf_tr_MeanTR","dti_jlf_fa_MeanWMFA","pcasl_jlf_cbf_MeanGMCBF","pcasl_jlf_cbf_MeanWMCBF", "pcasl_jlf_cbf_MeanWholeBrainCBF","rest_jlf_reho_MeanReho", "rest_jlf_alff_MeanALFF")
out.vals <- matrix(NA,nrow=length(global.val), ncol=21)
row.index <- 1
for(i in global.val){
  ## Create our formula
  form.val <- paste(i, "~perfBin+ageBin+sex")
  if(strSplitMatrixReturn(i, '_')[1]=='mprage'){
    form.val <- as.formula(paste(form.val,"+averageManualRating"))
  }
  if(strSplitMatrixReturn(i, '_')[1]=='Ct'){
      form.val <- as.formula(paste(form.val,"+averageManualRating"))
  }
  if(strSplitMatrixReturn(i, '_')[1]=='Ravens'){
      form.val <- as.formula(paste(form.val,"+averageManualRating"))
  }
  if(strSplitMatrixReturn(i, '_')[1]=='pcasl'){
      form.val <- as.formula(paste(form.val,"+pcaslRelMeanRMSMotion"))
  }
  if(strSplitMatrixReturn(i, '_')[1]=='dti'){
      form.val <- as.formula(paste(form.val,"+dti64Tsnr"))
  }
  if(strSplitMatrixReturn(i, '_')[1]=='rest'){
      form.val <- as.formula(paste(form.val,"+restRelMeanRMSMotion"))
  }
  mod <- lm(form.val, data=x)
  out.table <- anova(mod)
  out.table <- out.table[-5,]
  out.table$id <- rownames(out.table)
  to.write <- c(i, melt(out.table, id='id')$value)
  out.vals[row.index,1:length(to.write)] <- to.write
  row.index <- row.index+1
}
colnames(out.vals) <- c('globalVal', paste(melt(out.table, id='id')[,1], melt(out.table, id='id')[,2], sep='_'))
print(out.vals[which(as.numeric(out.vals[,18])<.05),'globalVal'])
write.csv(out.vals, "globalAnovRes.csv", quote=F, row.names=F)

## Now run a mem model
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
global.val <- c("mprage_jlf_vol_ICV","mprage_jlf_gmd_MeanGMD","pcasl_jlf_cbf_MeanWholeBrainCBF","dti_jlf_tr_MeanTR","dti_jlf_fa_MeanWMFA","pcasl_jlf_cbf_MeanGMCBF","rest_jlf_reho_MeanReho", "rest_jlf_alff_MeanALFF")
xTmpW <- x[,c(char.vec,global.val)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
mod <- lmerTest::lmer(value ~ ageBin+sex+perfBin*variable + (1|bblid), data=xTmpW, REML=FALSE)
to.write <- summary(mod)
write.csv(to.write$coefficients, "summaryGlobalMEM.csv", quote=F)
to.write <- anova(mod)
write.csv(to.write, "anovaSumGlobalMEM.csv", quote=F)

#########################################################################
# Run lobular models
# This will be done in two stages
# Stage 1 looks for any lobe*perfBin interaction
# Stage 2 looks for any 5 way perf*lobe*sex*age*laterality interaction
# STage 1 and Stage 2 use different data*
#########################################################################
grep.pat <- c("mprage_jlfHiLoLobe_vol", "mprage_jlfHiLoLobe_gmd", "pcasl_jlfHiLoLobe_cbf", "dti_jlfHiLoLobe_tr_","dti_dtitk_jhutract_fa","rest_jlfHiLoLobe_reho", "rest_jlfHiLoLobe_alff")
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin", "F1_Exec_Comp_Cog_Accuracy")
siglobes <- NULL
for(i in grep.pat){
  mod.pat <- strSplitMatrixReturn(charactersToSplit=i, splitCharacter='_')[,3]
  # First do white
  tmp <- names(x)[grep(i, names(x))]
  xTmpW <- x[,c(char.vec, tmp)]
  xTmpW <- melt(xTmpW, id.vars=char.vec)
  mod <- lmerTest::lmer(value ~ (ageAtScan1+sex+F1_Exec_Comp_Cog_Accuracy+variable)^2 + (1|bblid), data=xTmpW, REML=TRUE)
  tmp <- anova(mod,ddf="Kenward-Roger")
  write.csv(tmp, paste(i,".csv", sep=''), quote=F, row.names=F)
  ## Here see if we have a signivficant main effect of performance bin
  if(tmp['perfBin','Pr(>F)']<.05){
    ## Now print so Adon's knows we are here
    print(paste("Sig ME",i))
  }
  ## Now see if we have a significant perfBin:variable interaction
  if(tmp['perfBin:variable','Pr(>F)']<.05){
    ## Now print so Adon's knows we are here
    print(paste("Sig interaction",i))
    ## Now find the actually significnat ROI's
    tmp2 <- summary(mod)
    vals <- c(grep("perfBinme:variable", rownames(tmp2$coefficients)),grep("perfBinlo:variable", rownames(tmp2$coefficients)))
    to.add <- unique(rownames(tmp2$coefficients)[vals][which(as.numeric(tmp2$coefficients[vals,"Pr(>|t|)"]) <.05)])
    siglobes <- append(siglobes, to.add)
  }
}
# Now find our sig lobes
siglobes <- gsub(pattern="perfBinme:variable", replacement="", x=siglobes)
siglobes <- gsub(pattern="perfBinlo:variable", replacement="", x=siglobes)
siglobes <- unique(siglobes)

#########################################################################
# Run volume vals
#########################################################################
xVol <- averageLeftAndRightVol(x)
roi.names <- names(xVol)[c(115:121,519:584)]
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTmpW <- xVol[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now go through all of our significant lobes and run a LMER for each, within each
name.vec <- names(table(xTmpW$variable))
lobe.vec <- NULL
for(i in (names(table(xTmpW$variable)))){lobe.vec <- append(lobe.vec, findLobe(i))}
out.All <- matrix(nrow=15, ncol=0)
for(i in 1:9){
  to.include <- name.vec[which(lobe.vec==i)]
  ## Now run the model for the included regions
  mod.tmp <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+variable)^4 + (1|bblid), data=xTmpW[which(xTmpW$variable %in% to.include),], REML=FALSE)
  ## Now write the output values
  anova.vals <- anova(mod.tmp,ddf="Kenward-Roger")
  write.csv(anova.vals, paste("volumeAnova", i, ".csv", sep=''), quote=F, row.names=T)
  out.vals <- anova.vals[,c('F.value', 'Pr(>F)')]
  colnames(out.vals) <- paste(i, colnames(out.vals), sep='_')
  out.All <- cbind(out.All, out.vals)
  #sum.out <- summary(mod.tmp)$coefficients
  #write.csv(sum.out, paste("volumeSummary", i,".csv", sep=''), quote=F, row.names=T)
}
write.csv(out.All, "allSigLobeROIFValsVol.csv", quote=F, row.names=F)
#########################################################################
# Run TR vals
#########################################################################
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTR <- averageLeftAndRight(dataFrame=x)
roi.names <- names(xTR)[c(154:157,803:865)]
xTmpW <- xTR[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now go through all of our significant lobes and run a LMER for each, within each
name.vec <- names(table(xTmpW$variable))
lobe.vec <- NULL
for(i in (names(table(xTmpW$variable)))){lobe.vec <- append(lobe.vec, findLobe(i))}
out.All <- matrix(nrow=15, ncol=0)
for(i in c(1,2,3,4,5,6,7,8,9)){
  to.include <- name.vec[which(lobe.vec==i)]
  ## Now run the model for the included regions
  mod.tmp <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+variable)^4 + (1|bblid), data=xTmpW[which(xTmpW$variable %in% to.include),], REML=FALSE)
  ## Now write the output values
  anova.vals <- anova(mod.tmp,ddf="Kenward-Roger")
  write.csv(anova.vals, paste("trAnova", i, ".csv", sep=''), quote=F, row.names=T)
  out.vals <- anova.vals[,c('F.value', 'Pr(>F)')]
  colnames(out.vals) <- paste(i, colnames(out.vals), sep='_')
  out.All <- cbind(out.All, out.vals)
  sum.out <- summary(mod.tmp)$coefficients
  write.csv(sum.out, paste("trSummary", i,".csv", sep=''), quote=F, row.names=T)
}
write.csv(out.All, "allSigLobeROIFValsTr.csv", quote=F, row.names=F)
#########################################################################
# Run CBF vals
#########################################################################
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTR <- averageLeftAndRight(dataFrame=x)
roi.names <- names(xTR)[c(740:802)]
xTmpW <- xTR[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now go through all of our significant lobes and run a LMER for each, within each
name.vec <- names(table(xTmpW$variable))
lobe.vec <- NULL
for(i in (names(table(xTmpW$variable)))){lobe.vec <- append(lobe.vec, findLobe(i))}
out.All <- matrix(nrow=15, ncol=0)
for(i in c(1,2,3,4,5,6,7,9)){
  to.include <- name.vec[which(lobe.vec==i)]
  ## Now run the model for the included regions
  mod.tmp <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+variable)^4 + (1|bblid), data=xTmpW[which(xTmpW$variable %in% to.include),], REML=FALSE)
  ## Now write the output values
  anova.vals <- anova(mod.tmp,ddf="Kenward-Roger")
  write.csv(anova.vals, paste("trAnova", i, ".csv", sep=''), quote=F, row.names=T)
  out.vals <- anova.vals[,c('F.value', 'Pr(>F)')]
  colnames(out.vals) <- paste(i, colnames(out.vals), sep='_')
  out.All <- cbind(out.All, out.vals)
  sum.out <- summary(mod.tmp)$coefficients
  write.csv(sum.out, paste("cbfSummary", i,".csv", sep=''), quote=F, row.names=T)
}
write.csv(out.All, "allSigLobeROIFValsCBF.csv", quote=F, row.names=F)
#########################################################################
# Run Reho vals
#########################################################################
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTR <- averageLeftAndRight(dataFrame=x)
roi.names <- names(xTR)[c(175:177,1111:1167)]
xTmpW <- xTR[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now go through all of our significant lobes and run a LMER for each, within each
name.vec <- names(table(xTmpW$variable))
lobe.vec <- matrix(nrow=15, ncol=0)
for(i in (names(table(xTmpW$variable)))){lobe.vec <- append(lobe.vec, findLobe(i))}
out.All <- matrix(nrow=15, ncol=0)
for(i in c(1,2,3,4,5,6,7,8)){
  to.include <- name.vec[which(lobe.vec==i)]
  ## Now run the model for the included regions
  mod.tmp <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+variable)^4 + (1|bblid), data=xTmpW[which(xTmpW$variable %in% to.include),], REML=FALSE)
  ## Now write the output values
  anova.vals <- anova(mod.tmp,ddf="Kenward-Roger")
  write.csv(anova.vals, paste("rehoAnova", i, ".csv", sep=''), quote=F, row.names=T)
  out.vals <- anova.vals[,c('F.value', 'Pr(>F)')]
  colnames(out.vals) <- paste(i, colnames(out.vals), sep='_')
  out.All <- cbind(out.All, out.vals)
  sum.out <- summary(mod.tmp)$coefficients
  write.csv(sum.out, paste("rehoSummary", i, ".csv",sep=''), quote=F, row.names=T)
}
write.csv(out.All, "allSigLobeROIFValsReho.csv", quote=F, row.names=F)
#########################################################################
# Run ALFF vals
#########################################################################
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTR <- averageLeftAndRight(dataFrame=x)
roi.names <- names(xTR)[c(172:174,1054:1110)]
xTmpW <- xTR[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now go through all of our significant lobes and run a LMER for each, within each
name.vec <- names(table(xTmpW$variable))
lobe.vec <- matrix(nrow=15, ncol=0)
for(i in (names(table(xTmpW$variable)))){lobe.vec <- append(lobe.vec, findLobe(i))}
out.All <- matrix(nrow=15, ncol=0)
for(i in c(1,2,3,4,5,6,7,8)){
  to.include <- name.vec[which(lobe.vec==i)]
  ## Now run the model for the included regions
  mod.tmp <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+variable)^4 + (1|bblid), data=xTmpW[which(xTmpW$variable %in% to.include),], REML=FALSE)
  ## Now write the output values
  anova.vals <- anova(mod.tmp,ddf="Kenward-Roger")
  write.csv(anova.vals, paste("alffAnova", i, ".csv", sep=''), quote=F, row.names=T)
  out.vals <- anova.vals[,c('F.value', 'Pr(>F)')]
  colnames(out.vals) <- paste(i, colnames(out.vals), sep='_')
  out.All <- cbind(out.All, out.vals)
  sum.out <- summary(mod.tmp)$coefficients
  write.csv(sum.out, paste("alffSummary", i,  ".csv",sep=''), quote=F, row.names=T)
}
write.csv(out., "allSigLobeROIFValsAlff.csv", quote=F, row.names=F)
