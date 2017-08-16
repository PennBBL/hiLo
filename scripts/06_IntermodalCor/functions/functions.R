# find lobe function
# Should be used to place ROI into proper lobe
# Lobes will indlcude frontal, occipital, temporal, and occipital
# Also going to throw deep gm into the mix 
# Declare a function which will find the lobe value
findLobe <- function(grepPattern){
  # Declare the rois that we will grep through
  rois<-c("Thal","Putamen","Caudate","Pallidum",  # Basal Ganglia
          "Accumbens", "BasFor", # Basal Ganglia
          "PHG","Hip","PIns","SCA","AIns", # Limbic
          "ACgG","PCgG","Ent","Amygdala","MCgG", # Limbic
          "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe", # Frontal Orbital
          "FRP", "LOrG", # Frontal Orbital
          "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
          "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
          "PCu","PoG","AnG","PO","SPL","MPrG", # Parietal
          "SMG","MPoG", # Parietal
          "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG", # Occiptal
          "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X") # Cerebellum


  # Declare the index key which corresponds to lobe values
  index.key <- c(1,7,17,28,33,42,50,58,62)

  # Now find where the pattern matches to the roi list
  for(pattern.match.variable in 1:length(rois)){
    nuclei.to.grep <- rois[pattern.match.variable]
    grep.output <- grep(nuclei.to.grep ,grepPattern)    
    if(!identical(integer(0), grep.output)){
      break
    }
    if(pattern.match.variable==61){
      pattern.match.variable <- 62
    }
  }
  lobe.group <- findInterval(pattern.match.variable, index.key)
  return(lobe.group)
}

# Now create a function to return the column index individually amongst ROI's
findIndex <- function(grepPattern){
    # Declare the rois that we will grep through
    rois<-c("Thal","Putamen","Caudate","Pallidum",  # Basal Ganglia
    "Accumbens", # Basal Ganglia
    "PHG","Hip","PIns","SCA","AIns", # Limbic
    "ACgG","PCgG","Ent","Amygdala","MCgG", # Limbic
    "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe", # Frontal Orbital
    "FRP", "LOrG", # Frontal Orbital
    "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
    "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
    "PCu","PoG","AnG","PO","SPL","MPrG", # Parietal
    "SMG","MPoG", # Parietal
    "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG", # Occiptal
    "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X") # Cerebellum
    
    # Now find where the pattern matches to the roi list
    for(pattern.match.variable in 1:length(rois)){
        nuclei.to.grep <- rois[pattern.match.variable]
        grep.output <- grep(nuclei.to.grep ,grepPattern)
        if(!identical(integer(0), grep.output)){
            break
        }
    }
    # Now check for the MPoG
    mpogCheck <- grep('MPoG', grepPattern)
    if(length(mpogCheck) == 1){
      pattern.match.variable <- 48
    }
    mprgCheck <- grep('MPrG', grepPattern)
    if(length(mprgCheck) == 1){
      pattern.match.variable <- 46
    }
    ofugCheck <- grep('OFuG', grepPattern)
    if(length(ofugCheck) == 1 ){
      pattern.match.variable <- 52
    }
    lobe.group <- pattern.match.variable
    return(lobe.group)
}

# Now create a column index function for the CT DF
findIndexCT <- function(grepPattern){
    # Declare the rois that we will grep through
    rois<-c("PHG","PIns","SCA","AIns", # Limbic
    "ACgG","PCgG","Ent","MCgG", # Limbic
    "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe", # Frontal Orbital
    "FRP", "LOrG", # Frontal Orbital
    "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
    "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
    "PCu","PoG","AnG","PO","SPL","MPrG", # Parietal
    "SMG","MPoG", # Parietal
    "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG")
    
    # Now find where the pattern matches to the roi list
    for(pattern.match.variable in 1:length(rois)){
        nuclei.to.grep <- rois[pattern.match.variable]
        grep.output <- grep(nuclei.to.grep ,grepPattern)
        if(!identical(integer(0), grep.output)){
            break
        }
        
    }
    # Now check for the MPoG
    mpogCheck <- grep('MPoG', grepPattern)
    if(length(mpogCheck) == 1){
        pattern.match.variable <- 41
    }
    mprgCheck <- grep('MPrG', grepPattern)
    if(length(mprgCheck) == 1){
        pattern.match.variable <- 39
    }
    ofugCheck <- grep('OFuG', grepPattern)
    if(length(ofugCheck) == 1 ){
        pattern.match.variable <- 45
    }
    lobe.group <- pattern.match.variable
    return(lobe.group)
}

## Now create a function which will output a row of obe indices for an input data frame
outputLobeRow <- function(dataFrame){
  # Prime an output row
  na.row <- rep('NA', ncol(dataFrame))
  # Now create a value with all of the data frame names
  name.values <- names(dataFrame)
  # Now cycle thorugh the names and 
  for(col.index in 1:ncol(dataFrame)){
    val.to.grep <- name.values[col.index]
    lobeValue <- findLobe(val.to.grep)
    na.row[col.index] <- lobeValue
  }
  # Now return the row
  return(na.row)
}

## Now create a function which will reutnr individual column values
outputIndexRow <- function(dataFrame){
    # Prime an output row
    na.row <- rep('NA', ncol(dataFrame))
    # Now create a value with all of the data frame names
    name.values <- colnames(dataFrame)
    # Now cycle thorugh the names and
    for(col.index in 1:ncol(dataFrame)){
        val.to.grep <- name.values[col.index]
        lobeValue <- findIndex(val.to.grep)
        na.row[col.index] <- lobeValue
    }
    # Now return the row
    return(na.row)
}

outputIndexRowCT<- function(dataFrame){
    # Prime an output row
    na.row <- rep('NA', ncol(dataFrame))
    # Now create a value with all of the data frame names
    name.values <- colnames(dataFrame)
    # Now cycle thorugh the names and
    for(col.index in 1:ncol(dataFrame)){
        val.to.grep <- name.values[col.index]
        lobeValue <- findIndexCT(val.to.grep)
        na.row[col.index] <- lobeValue
    }
    # Now return the row
    return(na.row)
}

returnSigRMatrix <- function(dataFrame1, dataFrame2){
  rawCorMatrix <- cor(dataFrame1, dataFrame2, use="complete")
  sigValsMatrix <- corr.test(dataFrame1, dataFrame2)$p
  sigIndex <- apply(sigValsMatrix, 2, function(x){ifelse(x<0.05,1,NA)})
  multOutput <- sigIndex * rawCorMatrix
  return(multOutput)
}

plotHeatMap <- function(dataFrame1, dataFrame2, xlab, ylab, lowVal, highVal){
  # First prep the data
  tmp <- melt(cor(dataFrame1, dataFrame2, use="complete"))
  levels(tmp$Var1) <- rev(colnames(dataFrame1))
  levels(tmp$Var2) <- rev(colnames(dataFrame2))

  # Now find the minimum value
  #lowVal <- signif(min(tmp$value),digits=2) - .01
  #highVal <- signif(max(tmp$value),digits=2) + .01
  
  # Now create the image
  qplot(x=Var1, y=Var2, data=tmp, fill=value, geom="tile") + 
    #annotate("rect", ymin=.5, ymax=4.5, xmin= 0.5, xmax=4.5, fill=NA, color="black") + 
    #annotate("rect", ymin=4.5, ymax=12.5, xmin= 4.5, xmax=12.5, fill=NA, color="black") + 
    #annotate("rect", ymin=12.5, ymax=20.5, xmin= 12.5, xmax=20.5, fill=NA, color="black") + 
    #annotate("rect", ymin=20.5, ymax=34.5, xmin= 20.5, xmax=34.5, fill=NA, color="black") + 
    #annotate("rect", ymin=34.5, ymax=43.5, xmin= 34.5, xmax=43.5, fill=NA, color="black") + 
    #annotate("rect", ymin=43.5, ymax=61.5, xmin= 43.5, xmax=61.5, fill=NA, color="black") + 
    theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    xlab(xlab) +
    ylab(ylab) + 
    scale_fill_gradient2(low="blue", high="red", limits=c(lowVal, highVal))
}

plotSigHeatMap <- function(dataFrame1, dataFrame2, xlab, ylab, lowVal, highVal){
  # First prep the data
  tmp <- melt(returnSigRMatrix(dataFrame1, dataFrame2))
  levels(tmp$Var1) <- rev(colnames(dataFrame1))
  levels(tmp$Var2) <- rev(colnames(dataFrame2))

  # Now find the minimum value and max
  #lowVal <- signif(min(tmp$value, na.rm=T),digits=2) - .01
  #highVal <- signif(max(tmp$value, na.rm=T),digits=2) + .01

  # Now create the image
  qplot(x=Var1, y=Var2, data=tmp, fill=value, geom="tile") + 
    annotate("rect", ymin=.5, ymax=4.5, xmin= 0.5, xmax=4.5, fill=NA, color="black") + 
    annotate("rect", ymin=4.5, ymax=12.5, xmin= 4.5, xmax=12.5, fill=NA, color="black") + 
    annotate("rect", ymin=12.5, ymax=20.5, xmin= 12.5, xmax=20.5, fill=NA, color="black") + 
    annotate("rect", ymin=20.5, ymax=34.5, xmin= 20.5, xmax=34.5, fill=NA, color="black") + 
    annotate("rect", ymin=34.5, ymax=43.5, xmin= 34.5, xmax=43.5, fill=NA, color="black") + 
    annotate("rect", ymin=43.5, ymax=61.5, xmin= 43.5, xmax=61.5, fill=NA, color="black") + 
    theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    xlab(xlab) +
    ylab(ylab) + 
    scale_fill_gradient2(low="blue", high="red", limits=c(lowVal, highVal))    

}

# Create a function which will rm outliers given an input sd threshold
rmOutliers <- function(inputArray, SDcount){
  # First find the mean 
  inputMean <- mean(inputArray)
  # Now find the SD
  inputSD <- sd(inputArray)
  # Now find which values are below mean - (SDcount*SD)
  rmIndex <- which(inputArray < inputMean - (SDcount * inputSD)) 
  # Now find which are above
  rmIndex <- append(rmIndex, which(inputArray > inputMean - (SDcount * inputSD)))
  # Now return the input with the values outlier values masked 
  output <- inputArray
  output[rmIndex] <- 'NA'
  return(output)
}

# Now make a function to return signifianct corellations
returnSigRMatrix <- function(dataFrame1, dataFrame2){
  rawCorMatrix <- cor(dataFrame1, dataFrame2, use="complete")
  sigValsMatrix <- corr.test(dataFrame1, dataFrame2)$p
  sigIndex <- apply(sigValsMatrix, 2, function(x){ifelse(x<0.05,1,NA)})
  multOutput <- sigIndex * rawCorMatrix
  return(multOutput)
}

# Now create a function which will produce the heat maps and do everything ever
createHeatMap <- function(grepPattern1, grepPattern2){
  # First merge the two dataFrames
  matVals1 <- all.data[,grep(grepPattern1, names(all.data))]
  matVals2 <- all.data[,grep(grepPattern2, names(all.data))]

  # Now reorder the data 
  matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
  matVals2 <- matVals2[,order(outputLobeRow(matVals2))]

  #Now check to see if the modality is CT or cortcon
  ctCheck1 <- grep('ct', grepPattern1)
  ctCheck2 <- grep('ct', grepPattern2)
  ctCheck1 <- append(ctCheck1, grep('cortcon', grepPattern1))
  ctCheck2 <- append(ctCheck2, grep('cortcon', grepPattern2))

  # Now rm nonsense lobes
  if(length(ctCheck1)==1){
    matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
    matVals1 <- matVals1[,as.numeric(outputIndexRowCT(matVals1))]
    print('ctCheck Pass')
  }
  if(!length(ctCheck1)==1){
      matVals1 <- matVals1[,-which(outputLobeRow(matVals1)>7)]
      matVals1 <- matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
      matVals1 <- matVals1[,as.numeric(outputIndexRow(matVals1))]
  }
  if(length(ctCheck2)==1){
      matVals2 <- matVals2[,order(outputLobeRow(matVals2))]
      matVals2 <- matVals2[,as.numeric(outputIndexRowCT(matVals2))]
  }
  if(!length(ctCheck2)==1){
      matVals2 <- matVals2[,-which(outputLobeRow(matVals2)>7)]
      matVals2 <- matVals2[,order(outputLobeRow(matVals2))]
      matVals2 <- matVals2[,as.numeric(outputIndexRow(matVals2))]
  }

  # Now find the intersection of names and ensure that we only have regions that both DF have
  colNamesMV1 <- gsub(x=colnames(matVals1), pattern=grepPattern1, replacement='')
  colNamesMV2 <- gsub(x=colnames(matVals2), pattern=grepPattern2, replacement='')
  intersectVals <- intersect(colNamesMV1, colNamesMV2)
  lengthValue <- length(intersectVals)
  if(lengthValue!=dim(matVals1)[2]){
    matVals1 <- matVals1[,colNamesMV1 %in% intersectVals]
    print('mismatch')
  }  
  if(lengthValue!=dim(matVals2)[2]){
    print('mismatch')
    matVals2 <- matVals2[,colNamesMV2 %in% intersectVals]
  }
  # Now fix the names
  colnames(matVals1) <- gsub(x=colnames(matVals1), pattern=grepPattern1, replacement='')
  colnames(matVals2) <- gsub(x=colnames(matVals2), pattern=grepPattern2, replacement='')
  
  # Now find the modality name
  xAxisName <- toupper(strSplitMatrixReturn(grepPattern1, '_')[,3])
  print(xAxisName)
  yAxisName <- toupper(strSplitMatrixReturn(grepPattern2, '_')[,3])
  print(yAxisName)

  # Now plot our heat map!
  corMatrix <- cor(matVals1, matVals2, use='complete')
  maxVal <- max(corMatrix)
  minVal <- min(corMatrix)
  corMatrix <- melt(corMatrix)
  levels(corMatrix$Var1) <- colnames(matVals1)
  levels(corMatrix$Var2) <- levels(corMatrix$Var1)
  output <- qplot(x=Var1, y=Var2, data=corMatrix, fill=value, geom="tile") + 
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    labs(x = xAxisName, y=yAxisName) + 
    scale_fill_gradient2(low="blue", high="red", limits=c(minVal, maxVal)) + 
    coord_equal()
  output <- addRect(output, matVals1)
  return(output)  
}

# Now create a function which will produce the heat maps and do everything ever
createSigHeatMap <- function(grepPattern1, grepPattern2){
  # First merge the two dataFrames
  matVals1 <- all.data[,grep(grepPattern1, names(all.data))]
  matVals2 <- all.data[,grep(grepPattern2, names(all.data))]

  # Now reorder the data 
  matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
  matVals2 <- matVals2[,order(outputLobeRow(matVals2))]

  #Now check to see if the modality is CT
  ctCheck1 <- grep('ct', grepPattern1)
  ctCheck2 <- grep('ct', grepPattern2)
  ctCheck1 <- append(ctCheck1, grep('cortcon', grepPattern1))
  ctCheck2 <- append(ctCheck2, grep('cortcon', grepPattern2))

  # Now rm nonsense lobes
  if(length(ctCheck1)==1){
    matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
    matVals1 <- matVals1[,as.numeric(outputIndexRowCT(matVals1))]
    print('ctCheck Pass')
  }
  if(!length(ctCheck1)==1){
      matVals1 <- matVals1[,-which(outputLobeRow(matVals1)>7)]
      matVals1 <- matVals1 <- matVals1[,order(outputLobeRow(matVals1))]
      matVals1 <- matVals1[,as.numeric(outputIndexRow(matVals1))]
  }
  if(length(ctCheck2)==1){
      matVals2 <- matVals2[,order(outputLobeRow(matVals2))]
      matVals2 <- matVals2[,as.numeric(outputIndexRowCT(matVals2))]
  }
  if(!length(ctCheck2)==1){
      matVals2 <- matVals2[,-which(outputLobeRow(matVals2)>7)]
      matVals2 <- matVals2[,order(outputLobeRow(matVals2))]
      matVals2 <- matVals2[,as.numeric(outputIndexRow(matVals2))]
  }

  # Now find the intersection of names and ensure that we only have regions that both DF have
  colNamesMV1 <- gsub(x=colnames(matVals1), pattern=grepPattern1, replacement='')
  colNamesMV2 <- gsub(x=colnames(matVals2), pattern=grepPattern2, replacement='')
  intersectVals <- intersect(colNamesMV1, colNamesMV2)
  lengthValue <- length(intersectVals)
  if(lengthValue!=dim(matVals1)[2]){
    matVals1 <- matVals1[,colNamesMV1 %in% intersectVals]
    print('mismatch')
  }  
  if(lengthValue!=dim(matVals2)[2]){
    print('mismatch')
    matVals2 <- matVals2[,colNamesMV2 %in% intersectVals]
  }
  # Now fix the names
  colnames(matVals1) <- gsub(x=colnames(matVals1), pattern=grepPattern1, replacement='')
  colnames(matVals2) <- gsub(x=colnames(matVals2), pattern=grepPattern2, replacement='')
  
  # Now find the modality name
  xAxisName <- toupper(strSplitMatrixReturn(grepPattern1, '_')[,3])
  print(xAxisName)
  yAxisName <- toupper(strSplitMatrixReturn(grepPattern2, '_')[,3])
  print(yAxisName)

  # Now plot our heat map!
  corMatrix <- returnSigRMatrix(matVals1, matVals2)
  maxVal <- max(corMatrix)
  minVal <- min(corMatrix)
  corMatrix <- melt(corMatrix)
  levels(corMatrix$Var1) <- colnames(matVals1)
  levels(corMatrix$Var2) <- levels(corMatrix$Var1)
  output <- qplot(x=Var1, y=Var2, data=corMatrix, fill=value, geom="tile") + 
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    labs(x = xAxisName, y=yAxisName) + 
    scale_fill_gradient2(low="blue", high="red", limits=c(minVal, maxVal)) + 
    coord_equal()
  output <- addRect(output, matVals1)
  return(output)  
}

# Now cretae a function which will add the rectablges around the matrix values
addRect <- function(inputCorMatrix, inputMatVals1){
    # First find our lobular index values
    indexValues <- table(outputLobeRow(inputMatVals1))
    # Now go thorugh and create a vector with our values to draw our rectangles around
    indexVals <- c(.5)
    for(i in 1:length(unique(indexValues))){
      indexVals <- append(indexVals, unname(indexValues[i]))
    }
    indexVals <- indexVals
    # Now draw the rectangles
    tmpOut <- inputCorMatrix
    for(i in 2:max(as.numeric(names(indexValues)))){
      z <- i-1
      lowerLeft <- sum(indexVals[0:z])
      print(lowerLeft)
      upperRight <- sum(indexVals[1:i])
      if( is.na(upperRight) == TRUE ){
        upperRight <- dim(inputMatVals1)[2] + .5
      }
      print(upperRight)
      tmpOut <- tmpOut + annotate("rect", ymin=lowerLeft, ymax=upperRight, xmin=lowerLeft, xmax=upperRight, fill=NA, color='black', size=1.5)
    }
    output <- tmpOut
    return(output)
    
}
