# This is not a script to run, it is just a skeleton to follow to create the movies
# for the hi lo webpage.

# This for loop is what I used to put an overlay onotp of each png slice
for i in `ls -v *png` ; do  ~/Desktop/ffmpeg -i ${i} -i ~/Desktop/volumeHeader.png -y -filter_complex “overlay” ${i} ; done

# This guy actually makes the movie!
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vcodec libx264 -pix_fmt yuv440p tmp.mp4

# Each of these was run in the individual folder with the repsective images et cetra.


# Now build some loops which will create and organize each of these videos
modalityVals=(vol cbf gmd tr) #ct reho alff tr)
genderValues=(M F)
for g in ${genderValues[*]} ; do
  for m in ${modalityVals[*]} ; do
cd /Users/arose/Documents/forRuben/hiLo/data/hiMinusLoFigure/${g}/${m}/
rm ${m}.mp4
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vf scale=646:618 -vcodec libx264 -pix_fmt yuv420p ${m}2.mp4
ffmpeg -i ${m}2.mp4 -filter:v "setpts=2*PTS" ${m}.mp4
mv /Users/arose/Documents/forRuben/hiLo/data/hiMinusLoFigure/${g}/${m}/${m}.mp4 /Users/arose/Documents/adrose.github.io/hiLo/effectSize/${g}/
  done
done

# Now do the individual folds
modalityVals=(1 2 3 4)
genderValues=(1 2)
contrast=(2 3 5)
for s in ${contrast[*]} ; do
  for r in ${genderValues[*]} ; do
    for q in ${modalityVals[*]} ; do
cd /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/${s}/${r}/${q}/
if [[ ${q} == 1 ]] ; then
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vf scale=646:618 -vcodec libx264 -pix_fmt yuv420p vol2.mp4
ffmpeg -i vol2.mp4 -filter:v "setpts=2*PTS" vol.mp4
fi
if [[ ${q} == 2 ]] ; then
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vf scale=646:618 -vcodec libx264 -pix_fmt yuv420p cbf2.mp4
ffmpeg -i cbf2.mp4 -filter:v "setpts=2*PTS" cbf.mp4
fi
if [[ ${q} == 3 ]] ; then
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vf scale=646:618 -vcodec libx264 -pix_fmt yuv420p gmd2.mp4
ffmpeg -i gmd2.mp4 -filter:v "setpts=2*PTS" gmd.mp4
fi
if [[ ${q} == 4 ]] ; then
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vf scale=646:618 -vcodec libx264 -pix_fmt yuv420p md2.mp4
ffmpeg -i md2.mp4 -filter:v "setpts=2*PTS" md.mp4
fi
    done
  done
done

# Now mv all of the me - lo values
for q in ${modalityVals[*]} ; do
mv /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/2/1/${q}/*mp4 /Users/arose/Documents/adrose.github.io/hiLo/meLo/male/ ;
done
for q in ${modalityVals[*]} ; do
mv /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/2/2/${q}/*mp4 /Users/arose/Documents/adrose.github.io/hiLo/meLo/female/ ;
done

# Now onto the hi - me
for q in ${modalityVals[*]} ; do
mv /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/3/1/${q}/*mp4 /Users/arose/Documents/adrose.github.io/hiLo/hiMe/male/ ;
done
for q in ${modalityVals[*]} ; do
mv /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/3/2/${q}/*mp4 /Users/arose/Documents/adrose.github.io/hiLo/hiMe/female/ ;
done

# Now onto the lo - me
for q in ${modalityVals[*]} ; do
mv /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/5/1/${q}/*mp4 /Users/arose/Documents/adrose.github.io/hiLo/loMe/male/ ;
done
for q in ${modalityVals[*]} ; do
mv /Users/arose/Documents/forRuben/hiLo/data/imagingFigures/5/2/${q}/*mp4 /Users/arose/Documents/adrose.github.io/hiLo/loMe/female/ ;
done
