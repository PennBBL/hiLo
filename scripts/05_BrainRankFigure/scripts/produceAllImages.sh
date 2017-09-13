#!/bin/bash
# This is gong to be a quick and dirty script to produce all of the beta weight brain images for the hi lo project
baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
rData="${baseDir}rOutput/"
modalityVals=(vol cbf gmd tr) #ct reho alff tr)
genderValues=(Male Female)
contrast=(Hi Lo)
scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"

for c in ${contrast[*]} ; do
  for g in ${genderValues[*]} ; do 
    for m in ${modalityVals[*]} ; do 
      inputCsv="${rData}${m}${g}${c}-KEY.csv"
      mkdir -p ${baseDir}/imagingFigures/${c}/Cortical/${g}/${m}/ 
      cd ${baseDir}/imagingFigures/${c}/Cortical/${g}/${m}/ 
      ${scriptName} ${inputCsv} 4 1 & 
    done
  done
done
exit 66
