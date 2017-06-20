returnHeatMapITKSnapVals <- function(inputZScores, lowColor='blue', hiColor='red'){
    # Create some functions this function will call... yeesh
    range01 <- function(x){
        # Now make sure we have some standard deviation
        # If no standard deviation return 1
        if( is.na(sd(x)) == 'TRUE'){
            output <- rep(1, length(x))
            return(output)
        }
        else if (sd(x) < 0 ){
            output <- rep(1, length(x))
            return(output)
        }
        else
        (x-min(x))/diff(range(x))
    }
    cRamp <- function(x){
        cols <- colorRamp(c(lowColor, hiColor))(range01(as.numeric(x)))
    }
    # Output values
    outputValues <- matrix(0, nrow=(length(inputZScores)+1), ncol=8)
    
    # Now cretae our rgb values
    redValues <- round(cRamp(inputZScores)[,1], digits=0)
    greenValues <- round(cRamp(inputZScores)[,2], digits=0)
    blueValues <- round(cRamp(inputZScores)[,3], digits=0)
    
    # Now create our index column
    outputValues[,1] <- seq(0, length(inputZScores))
    
    # Now put the proper values in the correct place
    outputValues[2:(length(inputZScores)+1),2] <- redValues
    outputValues[2:(length(inputZScores)+1),3] <- greenValues
    outputValues[2:(length(inputZScores)+1),4] <- blueValues
    
    # Now we need to do the Transperancy column
    outputValues[,5] <- c(0, rep(1, length(inputZScores)))
    
    # Now the visibility column
    outputValues[,6] <- c(0, rep(1, length(inputZScores)))
    
    # Now the mesh visibility
    outputValues[,7] <- c(0, rep(1, length(inputZScores)))
    
    # Now the label indicies
    labelIndexNames <- c('Clear Label', paste('Label ', inputZScores, sep=''))
    labelIndexNames <- paste('"', labelIndexNames, '"', sep='')
    outputValues[,8] <- labelIndexNames
    
    # Now return our output
    return(outputValues)
}

returnPosNegAndNeuColorScale <- function(outputZScores, colorScaleNeg=c('light blue', 'blue'), colorScalePos=c('yellow', 'red'), colorScaleNeu=c('gray'), sigThreshold=.05){
    # MAKE SURE WE ARE DEALING WITH NUMERICS!!!!
    outputZScores <- as.numeric(as.character(outputZScores))
    
    # First convert our sig threshold into a z score to find our cut off value
    cutOff <- abs(qnorm(sigThreshold))
    
    # Now we need to make our seperate our data into neutral, positive, and negative values
    # We are going to order these just so it is easier to match the labesl to the output ROI
    # when working with the ouput of this function
    negativeValues <- outputZScores[which(outputZScores < 0)]
    negativeValues <- negativeValues[order(negativeValues)]
    positiveValues <- outputZScores[which(outputZScores >= 0)]
    positiveValues <- positiveValues[order(positiveValues)]
    
    # Create our blank label row first
    values <- rep(0, 7)
    blankRow <- append(values, paste('"', 'Clear Label' ,'"', sep=''))
    
    # Now we need to create our individual color scales
    #startPoint <- NULL
    output <- blankRow
    if(length(negativeValues) > 0 ){
        negativeColors <- returnHeatMapITKSnapVals(negativeValues, lowColor=colorScaleNeg[1], hiColor=colorScaleNeg[2])[2:(length(negativeValues)+1),]
        #negIndex <- max(as.numeric(as.character(negativeColors[,1])))
        #startPoint <- cbind(startPoint, negIndex)
        output <- rbind(output, negativeColors)
    }
    if(length(positiveValues) > 0 ){
        positiveColors <- returnHeatMapITKSnapVals(positiveValues, lowColor=colorScalePos[1], hiColor=colorScalePos[2])[2:(length(positiveValues)+1),]
        #posIndex <- max(as.numeric(as.character(positiveColors[,1])))
        #startPoint <- cbind(startPoint, posIndex)
        output <- rbind(output, positiveColors)
    }
    # Now I need to make sure that the index column doesn't have any repeats
    # This will be done by running an an index thorugh the first column
    output[,1] <- seq(0, length(outputZScores))
    
    # Now we are all set! just need to return our output
    return(output)
}

# Declare a function to write the table and key
writeColorTableandKey <- function(inputData, inputColumn, outName){
  # First create the color table
  tmpColorTable <- returnPosNegAndNeuColorScale(inputData[complete.cases(inputData[,inputColumn]),inputColumn], colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('yellow', 'red'), sigThreshold=1)
  valuesToBind <- c('1616', '190', '190', '190', '0.40', '1', '1', 'Label Nonsense')

  # Now produce the output key 
  tmpOutputKey <- matrix(NA, nrow=dim(tmpColorTable)[1]-1, ncol=3)
  tmpOutputKey[,1] <- as.character(inputData[complete.cases(inputData[,inputColumn]),1])
  tmpOutputKey[,2] <- as.character(inputData[complete.cases(inputData[,inputColumn]),inputColumn])
  tmpOutputKey[,3] <- seq(dim(tmpColorTable)[1]-1, 1, -1)
  tmpOutputKeyFlip <- tmpOutputKey
  tmpOutputKey[,1] <- paste('R_', tmpOutputKey[,1], sep='')
  tmpOutputKeyFlip[,1] <- paste('L_', tmpOutputKeyFlip[,1], sep='')

  # Now give the wm a L_ prefix
  #if(length(grep('WM', tmpOutputKey[,1])) > 0){
  #  indx <- grep('WM', tmpOutputKey[,1])
  #  tmpOutputKey[indx,1] <- gsub( x= tmpOutputKey[indx,1], pattern='R_', replacement='L_')
  #  
  #}  
  # Now write the tables
  outCTName <- paste(outName, '-ColorTable.txt', sep='')
  outKeyName <- paste(outName, '-KEY.csv', sep='')
  tmpColorTable <- rbind(tmpColorTable, valuesToBind)
  write.table(tmpColorTable, file=outCTName, sep="\t", quote=F, row.names=F, col.names=F)
  tmpOutputKey <- rbind(tmpOutputKey, tmpOutputKeyFlip)
  write.csv(tmpOutputKey, file=outKeyName, quote=F)
}
