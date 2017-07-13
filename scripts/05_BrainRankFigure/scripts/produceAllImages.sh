#!/bin/bash


# This is gong to be a quick and dirty script to produce all of the beta weight brain images for the hi lo project

baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
rData="${baseDir}rOutput/"
modalityVals=(vol cbf gmd) #ct reho alff tr)
genderValues=(Male Female)
scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"

for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do 
    inputCsv="${rData}${m}${g}-KEY.csv"
    ${scriptName} ${inputCsv} 4 1 Cortical
    mkdir -p ${baseDir}/imagingFigures/Cortical/${g}/${m}/ 
    mv ./outputImage.nii.gz ${baseDir}/imagingFigures/Cortical/${g}/${m}/ ; 
  done
done

for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do 
    inputCsv="${rData}${m}${g}-KEY.csv"
    ${scriptName} ${inputCsv} 4 1 DGM 
    mkdir -p ${baseDir}/imagingFigures/DGM/${g}/${m}/ 
    mv ./outputImage.nii.gz ${baseDir}/imagingFigures/DGM/${g}/${m}/ ; 
  done
done

for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do 
    inputCsv="${rData}${m}${g}-KEY.csv"
    ${scriptName} ${inputCsv} 4 1 WM 
    mkdir -p ${baseDir}/imagingFigures/WM/${g}/${m}/ 
    mv ./outputImage.nii.gz ${baseDir}/imagingFigures/WM/${g}/${m}/ ; 
  done
done
