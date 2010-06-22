# usage
# ./converting.sh inp 14 18 pgm14_18
# converts all images (jpg files) of "inp" dir into pgm (portable grayscale) in the folder pgm14_18$
# use directories without slash !
# original files have are 70px weight x 90px height

width=$2
height=$3
res=$2x$3!

srcdir=$1
outputdir=$4
mkdir $outputdir

#for i in $(find $srcdir -name "*.jpg" -depth 1); do
for i in $(find $srcdir -name "*.jpg"); do
        filename=${i//$srcdir\//""}
        new_file=${filename//jpg/pgm}
        #echo $srcdir/$filename wird zu $outputdir/$new_file
        convert $srcdir/$filename -resize $res -compress none $outputdir/$new_file
        #convert A.jpg -resize 35x45! -compress none A_big.pgm
done
