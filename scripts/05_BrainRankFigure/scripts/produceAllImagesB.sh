#!/bin/bash


# This is gong to be a quick and dirty script to produce all of the beta weight brain images for the hi lo project

baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
rData="${baseDir}rOutput/"
modalityVals=(vol cbf gmd ct reho alff tr)
genderValues=(Male feMale)
scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"

for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do 
    inputCsv="${rData}${m}${g}-KEY.csv"
    ${scriptName} ${inputCsv} 3
    mkdir -p ${baseDir}/imagingFigures/${g}2/${m}/ 
    mv ./outputImage.nii.gz ${baseDir}/imagingFigures/${g}/${m}/ ; 
  done
done
    
