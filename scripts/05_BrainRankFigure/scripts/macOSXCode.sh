# This is not a script to run, it is just a skeleton to follow to create the movies
# for the hi lo webpage.

# This for loop is what I used to put an overlay onotp of each png slice
for i in `ls -v *png` ; do  ~/Desktop/ffmpeg -i ${i} -i ~/Desktop/volumeHeader.png -y -filter_complex “overlay” ${i} ; done

# This guy actually makes the movie!
~/Desktop/ffmpeg -i axial%4d.png -y -b 2M -framerate 10 -filter:v "setpts=2.0*PTS" -vcodec libx264 -pix_fmt yuv440p tmp.mp4

# Each of these was run in the individual folder with the repsective images et cetra.
