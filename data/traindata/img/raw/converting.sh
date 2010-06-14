# usage
# ./converting.sh 14 18 pgm14_18
# converts all images (jpg files) of current path into pgm (portable grayscale) in the folder pgm14_18 with a rescale: width=14 height=18
# original files have are 70px weight x 90px height 

width=$1
height=$2
res=$1x$2!

outputdir=$3
mkdir $outputdir

for i in $(find . -name "*.jpg"); do
	filename=${i//.\//""}
	new_file=${filename//jpg/pgm}
	#echo $filename wird zu $new_file
	convert $filename -resize $res -compress none $outputdir/$new_file
	#convert A.jpg -resize 35x45! -compress none A_big.pgm
done
