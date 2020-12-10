fdir1=`ls $1/*.c | wc -l`
fdir2=`ls $2/*.c | wc -l`
totbr=`expr $fdir1 + $fdir2`
if [ $totbr -gt 20 ]
then
  ls $1/*.c > filenames
  ls $2/*.c >> filenames
  chmod 0444 filenames
else
  echo "Total number of files less than 20 is $totbr"
fi 
