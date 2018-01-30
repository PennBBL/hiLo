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

# Now do the beta weight images down here
baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
rData="/home/arosen/hiLo/data/05_BrainRankFigure/betaWeight/"
modalityVals=(vol.data cbf.data gmd.data tr.data alff.data reho.data)
genderValues=(Male Female)
scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"
for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do
    inputCSV="${rData}${m}.${g}-KEY.csv"
    mkdir -p ${baseDir}/imagingFiguresBeta/${g}/${m}/
    cd ${baseDir}/imagingFiguresBeta/${g}/${m}/
    ${scriptName} ${inputCSV} 3 0 &
  done
done
for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do
    inputCSV="${rData}${m}.${g}-KEY.csv"
    mkdir -p ${baseDir}/imagingFiguresBetaITK/${g}/${m}/
    cd ${baseDir}/imagingFiguresBetaITK/${g}/${m}/
    ${scriptName} ${inputCSV} 4 1 &
  done
done 
exit 67

# Now finally the hi - lo images -_-
baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/hiMinusLoFigure/"
rData="/home/arosen/hiLo/data/05_BrainRankFigure/hiMinusLoFigure/input/"
modalityVals=(vol cbf gmd tr)
genderValues=(M F)
scriptName="/home/arosen/pncMJPS/scripts/09_brainImages/scripts/makeZScoreJLFPNCTemplateImage.sh"
for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do 
    inputCSV="${rData}${g}${m}-KEY.csv"
    mkdir -p ${baseDir}${g}
    ${scriptName} ${inputCSV} 4 1 
    mv outputImage.nii.gz ${baseDir}${g}/${m}.nii.gz
  done
done
exit 68
