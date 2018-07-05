## AFGR 

## This script will be used to prepare the functional connection strength for the ps vs resist and td vs emergent classification
## Super rough and quick script

## Declare any static variables
source('/home/arosen/adroseHelperScripts/R/afgrHelpFunc.R')
basePath <- "/data/joy/BBL/studies/pnc/processedData/restbold/restbold_201607151621/"
exclusion.vals <- read.csv('/data/joy/BBL/studies/pnc/n2416_dataFreeze/neuroimaging/rest/n2416_RestQAData_20170714.csv')
subjectExclude <- exclusion.vals[which(exclusion.vals$restExclude==1),'scanid']
## Now lets load our subject id's
inputCSV <- c("tdVsEmergSubj.csv", "psVsResistSubj.csv")

## Now in a loop go through every subject identifier and see if we have a file for them
for(i in inputCSV){
  subjectvals <- read.csv(i)
  colnames(subjectvals)[1] <- 'bblid'
  ## Now for every subject value go through and see if we have edge weights we can use
  subjectvals$Include <- 1
  subjectvals[subjectvals[,2] %in% subjectExclude,'Include'] <- 0
  subjectvals <- subjectvals[which(subjectvals$Include==1),]
  ## Now for every included subject
  ## Lets go and find their edge weights
  pathname <- paste(basePath, subjectvals[,1], "/*",subjectvals[,2],"/net/Schaefer100PNC/",subjectvals[,1], "*", subjectvals[,2],"_Schaefer100PNC.net", sep='')
  ## Now loop through every subject
  files.out <- NULL
  for(filename in pathname){
    to.call <- paste("ls", filename)
    to.read <- system(to.call, intern=T)
    edge.weights <- read.table(to.read, skip=2)
    # Now update the data so we can combine them all
    new.values <- t(edge.weights)
    colnames(new.values) <- apply(new.values[1:2,], 2, function(x) paste(x[1], x[2], sep='x'))
    new.values <- new.values[-c(1:2),]
    new.values$bblid <- strSplitMatrixReturn(filename, "/")[,10]
    files.out <- rbind(files.out, new.values)
  }
  # Now get our outcome
  files.out <- as.data.frame(files.out)
  to.write <- merge(subjectvals, files.out, by='bblid')
  to.write <- to.write[,-c(1,2,4)]
  to.write <- apply(to.write,2,as.character)
  write.csv(to.write, paste("edgeWeights", i, sep=''), quote=F, row.names=F)
}
