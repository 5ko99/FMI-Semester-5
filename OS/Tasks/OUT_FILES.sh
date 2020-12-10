fdir1=`ls $1/*.out | wc -l`
fdir2=`ls $2/*.out | wc -l`
if [ $fdri1 -gt $fdir2 ]
then
  mkdir NEWdir
  echo NEWdir
  for i in $1/*
  do
    if test -f $i -a -r $i -a -w $i
    then
      mv $i NEWdir
    fi
  done
fi
