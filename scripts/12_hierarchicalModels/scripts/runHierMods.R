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
source('./prepareLaterHiLoLobes.R')
x <- merge(x, all.out)
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

## Now run a mem model
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
global.val <- c("mprage_jlf_vol_ICV","mprage_jlf_gmd_MeanGMD","pcasl_jlf_cbf_MeanWholeBrainCBF","dti_jlf_tr_MeanTR","dti_jlf_fa_MeanWMFA","pcasl_jlf_cbf_MeanGMCBF","rest_jlf_reho_MeanReho", "rest_jlf_alff_MeanALFF")
xTmpW <- x[,c(char.vec,global.val)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
mod <- lmerTest::lmer(value ~ ageBin+sex+perfBin*variable + (1|bblid), data=xTmpW, REML=FALSE)

#########################################################################
# Run lobular models
# This will be done in two stages
# Stage 1 looks for any lobe*perfBin interaction
# Stage 2 looks for any 5 way perf*lobe*sex*age*laterality interaction
# STage 1 and Stage 2 use different data*
#########################################################################
grep.pat <- c("mprage_jlfHiLoLobe_vol", "mprage_jlfHiLoLobe_gmd", "pcasl_jlfHiLoLobe_cbf", "dti_jlfHiLoLobe_tr_","dti_dtitk_jhutract_fa","rest_jlfHiLoLobe_reho", "rest_jlfHiLoLobe_alff")
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
siglobes <- NULL
for(i in grep.pat){
  mod.pat <- strSplitMatrixReturn(charactersToSplit=i, splitCharacter='_')[,3]
  # First do white
  tmp <- names(x)[grep(i, names(x))]
  xTmpW <- x[,c(char.vec, tmp)]
  xTmpW <- melt(xTmpW, id.vars=char.vec)
  mod <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+variable)^2 + (1|bblid), data=xTmpW, REML=FALSE)
  tmp <- anova(mod,ddf="Kenward-Roger")
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

## Now run the 5 way interactions
grep.pat <- c("mprage_vol_rois", "dti_tr_rois","dti_dtitk_jhutract_fa","rest_reho_rois_", "rest_alff_rois_")
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
model.summary <- NULL
for(i in grep.pat){
  mod.pat <- strSplitMatrixReturn(charactersToSplit=i, splitCharacter='_')[,3]
  # First do white
  tmp <- names(x)[grep(i, names(x))]
  xTmpW <- x[,c(char.vec, tmp)]
  xTmpW <- melt(xTmpW, id.vars=char.vec)
  # Now add a laterality factor
  xTmpW$Laterality <- 'R'
  xTmpW$Laterality[which(strSplitMatrixReturn(charactersToSplit=xTmpW$variable, splitCharacter='_')[,4]=='L')] <- 'L'
  if(i =="dti_dtitk_jhutract_fa"){
    # Now I need declare the jhu tract lobes
    # This is a little different from the JLF process
    # First declare a function to reverse characters
    strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
    xTmpW$Laterality <- NA
    xTmpW$Laterality[strSplitMatrixReturn(strReverse(as.character(xTmpW$variable)), "_")[,1]=='r'] <- 'R'
    xTmpW$Laterality[strSplitMatrixReturn(strReverse(as.character(xTmpW$variable)), "_")[,1]=='l'] <- 'L'
  }
  xTmpW$Laterality <- as.factor(xTmpW$Laterality)
  ## Now create a new ROI w/o lobe in it
  xTmpW$Lobe <- strSplitMatrixReturn(xTmpW$variable, splitCharacter='_')[,5]
  mod <- lmerTest::lmer(value ~ ageBin*sex*perfBin*Lobe*Laterality + (1|bblid), data=xTmpW, REML=FALSE)
  tmp <- anova(mod,ddf="Kenward-Roger")
  ## Now print the anova
  model.summary <- rbind(model.summary, t(melt(tmp))['value',])  
}
## Now fix the column names
tmp$colnames <- rownames(tmp)
twerk <- melt(tmp, id='colnames')
colnames(model.summary) <- paste(twerk$colnames, twerk$variable, sep='')

#########################################################################
# Run volume vals
#########################################################################
roi.names <- names(x)[c(117:120,122:127,129:130,135:253)]
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTmpW <- x[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now add a laterality component
xTmpW$Laterality <- 'NA'
xTmpW$Laterality[grep("_L_", xTmpW$variable)] <- 'L'
xTmpW$Laterality[grep("_R_", xTmpW$variable)] <- 'R'
# Now add a hemi ignorant ROI variable
xTmpW$ROI <- xTmpW$variable
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='mprage_jlf_vol_', replacement='')
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='L_', replacement='')
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='R_', replacement='')
mod <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+ROI+Laterality)^5 + (1|bblid), data=xTmpW, REML=FALSE)

#########################################################################
# Run TR vals
#########################################################################
sig.lobe.rois <- c(rois_L_FrOrb, rois_R_FrOrb, rois_L_Tem, rois_R_Tem, rois_L_Occ, rois_R_Occ, rois_L_Par, rois_R_Par, names(x)[841:852])
roi.names <- gsub(x=sig.lobe.rois, pattern='mprage', replacement='dti')
roi.names <- gsub(x=roi.names, pattern='vol', replacement='tr')
roi.names[56] <- 'dti_jlf_tr_L_GRe'
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTmpW <- x[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now add a laterality component
xTmpW$Laterality <- 'NA'
xTmpW$Laterality[grep("_L_", xTmpW$variable)] <- 'L'
xTmpW$Laterality[grep("_R_", xTmpW$variable)] <- 'R'
# Now add a hemi ignorant ROI variable
xTmpW$ROI <- xTmpW$variable
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='dti_jlf_tr_', replacement='')
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='L_', replacement='')
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='R_', replacement='')
mod <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+ROI+Laterality)^5 + (1|bblid), data=xTmpW, REML=FALSE)

#########################################################################
# Run Reho vals
#########################################################################
sig.lobe.rois <- c(rois_L_FrDors, rois_R_FrDors, rois_L_Par, rois_R_Par, names(x)[841:852])
roi.names <- gsub(x=sig.lobe.rois, pattern='mprage', replacement='rest')
roi.names <- gsub(x=roi.names, pattern='vol', replacement='reho')
char.vec <- c("bblid","sex","ageAtScan1", "race2","perfBin","ageBin")
xTmpW <- x[,c(char.vec,roi.names)]
xTmpW <- melt(xTmpW, id.vars=char.vec)
## Now add a laterality component
xTmpW$Laterality <- 'NA'
xTmpW$Laterality[grep("_L_", xTmpW$variable)] <- 'L'
xTmpW$Laterality[grep("_R_", xTmpW$variable)] <- 'R'
# Now add a hemi ignorant ROI variable
xTmpW$ROI <- xTmpW$variable
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='dti_jlf_tr_', replacement='')
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='L_', replacement='')
xTmpW$ROI <- gsub(x=xTmpW$ROI, pattern='R_', replacement='')
mod <- lmerTest::lmer(value ~ (ageBin+sex+perfBin+ROI+Laterality)^5 + (1|bblid), data=xTmpW, REML=FALSE)
