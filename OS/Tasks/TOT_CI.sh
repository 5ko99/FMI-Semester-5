fdir1=`ls $1/*.c | wc -l`
fdir2=`ls $2/*.c | wc -l`
let totbr=$fdir1+$fdir2
echo $totbr
if [ $totbr -gt 4 ]
then
  ls $1/*.c > filenames
  ls $2/*.c > filenames
  chmod 0444 filenames
else
  echo "Total number of files less than 20 is $totbr"
fi 
