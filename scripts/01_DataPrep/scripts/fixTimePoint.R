library('reshape2')
all.data <- read.csv('./n2416_imagingDataDump_2018-04-04.csv')

# Now find all of our subjects with NA timepoints
na.vals <- dcast(data=all.data, bblid~timepoint)
na.subjs.index  <- which(na.vals[,5]==1)
# Now correct the subjects w/o TP 1 values
tp.one.vals <- na.vals[which(na.vals[,2]==0 & na.vals[,5]==1),1]
tp.one.vals <- c(tp.one.vals, na.vals[which(na.vals[,2]==0 & na.vals[,5]==2),1])[-62]
tp.two.vals <- na.vals[which(na.vals[,2]==1 & na.vals[,5]==1 & na.vals[,3]==0),1]
tp.two.vals <- append(tp.two.vals, na.vals[which(na.vals[,2]==1 & na.vals[,5]==2 & na.vals[,3]==0),1])
tp.three.vals <- na.vals[which(na.vals[,2]==1 & na.vals[,5]==1 & na.vals[,3]==1 & na.vals[,4]==0),1]
tp.three.vals <- append(tp.three.vals, '89115')

# Now we need to go through the subjects and modify the correct tp to the
# correct tp value

# I am going to do this in a loop, find the minimum scanid value
# and then modify the appropriate TP value
table(all.data$timepoint)
for(b in tp.one.vals){
    # Find the minimum scanid value
    scanidVals <- all.data[which(all.data$bblid==b),'scanid']
    minSID <- min(scanidVals)
    all.data[which(all.data$bblid==b & all.data$scanid==minSID),'timepoint'] <- 1
}
table(all.data$timepoint)

# Now do TP2
table(all.data$timepoint)
for(b in tp.two.vals){
    # Find the next scanid larger than the minimum
    scanidVals <- all.data[which(all.data$bblid==b),'scanid']
    if(length(scanidVals)==2){
      cSID <- max(scanidVals)
    }
    if(length(scanidVals)==3){
      print('foo')
      cSID <- median(scanidVals)
    }
    all.data[which(all.data==b & all.data$scanid==cSID),'timepoint'] <- 2
}
table(all.data$timepoint)

# Now do TP3
table(all.data$timepoint)
for(b in tp.three.vals){
    # Find the minimum scanid value
    scanidVals <- all.data[which(all.data$bblid==b),'scanid']
    minSID <- max(scanidVals)
    all.data[which(all.data$bblid==b & all.data$scanid==minSID),'timepoint'] <- 3
}
table(all.data$timepoint)


# Awesome now we have a more correct tp variable!
# lets check this first though...
ch.vals <- dcast(data=all.data, bblid~timepoint)

## Looks like we had a couple of nefarious subjects
## time to make some manual corrections =(
bad.bblid <- ch.vals[which(ch.vals[,5]!=0),]
all.data[which(all.data$bblid==bad.bblid[1,1] & is.na(all.data$timepoint)),'timepoint'] <- 2
all.data[which(all.data$bblid==bad.bblid[2,1] & is.na(all.data$timepoint)),'timepoint'] <- 2
all.data[which(all.data$bblid==bad.bblid[3,1] & is.na(all.data$timepoint)),'timepoint'] <- c(1,2)
all.data$tpvalue <- all.data$timepoint
all.data <- all.data[,-which(names(all.data)=='timepoint')]

## Now I need to try and merge the cognitive data again by timepoint
## Which means I have to load all of the cognitive data, and remove the cognitive data
## in the current all.data frame as it stands right now
all.data.sub <- all.data[,-c(1620:1632,1640:1704)]
all.data.sub$DOSCAN <- as.character(all.data.sub$DOSCAN)
all.data.sub$DOSCAN <- as.Date(all.data.sub$DOSCAN, "%m/%d/%y")
## Now age regress these values
regressOutAge <- function(valuesToBeRegressed, ageColumn){
    age <- scale(ageColumn)[,1]
    ageSqu <- scale(ageColumn)[,1]^2
    ageCub <- scale(ageColumn)[,1]^3
    index <- as.numeric(names(residuals(lm(valuesToBeRegressed ~ age + ageSqu + ageCub))))
    newValues <- rep(NA, length(valuesToBeRegressed))
    newValues[index] <- lm(valuesToBeRegressed ~ age + ageSqu + ageCub)$residuals
    return(newValues)
}
all.data.sub[,grep("jlf", names(all.data.sub))] <- apply(all.data.sub[,grep("jlf", names(all.data.sub))], 2, function(x) regressOutAge(x, all.data.sub$scanageMonths))
# Now I need to calculate the time since the last scan
all.data.sub$timeDiffScan <- 0
# Now loop through each BBLID and find the time since time 1
all.data.sub.out <- NULL
for(i in unique(all.data.sub$bblid)){
    ## Now add days from Scan 1
    tmpDat <- all.data.sub[which(all.data.sub$bblid==i),]
    print(dim(tmpDat))
    if(dim(tmpDat)[1]>1){
        minValue <- min(tmpDat$DOSCAN)
        tmpDat$timeDiffScan <- difftime(tmpDat$DOSCAN, minValue, units=c("day"))
    }
    # Now combine everything
    all.data.sub.out <- rbind(all.data.sub.out, tmpDat)
}

## Now load all of the cog and clinical data
cog.data <- read.csv('./CNB_Factor_Scores_GO1-GO2-GO3_wdates.csv')
cog.data$cnbDate <- as.Date(cog.data$cnbDate, "%m/%d/%y")
cog.data$tpvalue <- 0
cog.data$timeDiffCNB <- 0
## Now I need to add a timepoint variable to this data frame
output.cog <- NULL
for(i in unique(cog.data$bblid)){
    ## Now add days from Scan 1
    tmpDat <- cog.data[which(cog.data$bblid==i),]
    if(dim(tmpDat)[1]>1){
        minValue <- min(tmpDat$cnbDate)
        timeDiff <- difftime(tmpDat$cnbDate, minValue, units=c("day"))
        rankVals <- rank(timeDiff)
    }
    else{
        rankVals <- 1
        timeDiff <- 0
    }
    tmpDat$tpvalue <- rankVals
    tmpDat$timeDiffCNB <- timeDiff
    # Now combine everything
    output.cog <- rbind(output.cog, tmpDat)
}

## Now we need to do the same with the clinical values
cln.data <- read.csv('./longFacScore.csv')
names(cln.data)[2] <- 'tpvalue'

## Now merge our cog and clin factor scores
output.cog <- merge(output.cog, cln.data, all=T)

## Now combine this with our imaging data
all.data.sub <- merge(all.data.sub, output.cog, by=c('bblid', 'tpvalue'), all=T)
#all.data.sub <- all.data.sub[-which(is.na(all.data.sub$scanid)),]

## Now attach the long labels
long.labels <- read.csv('./pnc_diagnosis_categorical_20170526.csv')
all.data.sub <- merge(all.data.sub, long.labels, all=T)
#all.data.sub <- all.data.sub[-which(is.na(all.data.sub$scanid)),]

## Now add the prs score
prs.score <- read.csv('./PRS_Adon.csv')
all.data.sub$PRS <- prs.score$PRS[match(all.data.sub$bblid, prs.score$bblid)]

ers.score <- read.csv('./n9498_go1_environment_factor_scores_tymoore_20150909.csv')
all.data.sub <- merge(all.data.sub, ers.score)

## Now reshape this nonsense....
## dis gun be big
all.data.sub <- all.data.sub[-which(is.na(all.data.sub$tpvalue)),]
out.data <- reshape(all.data.sub, idvar="bblid", timevar="tpvalue", direction="wide")
write.csv(out.data, "~/Desktop/superMegaData.csv", quote=F, row.names=F)






