if test ! -d $HOME/$1
then
  mkdir $HOME/$1
fi
num=0
for var in *
do
  if  test -f $var -a -r $var
  then
    cp $var $HOME/$1
  else
    let num=$num+1
  fi
done
echo "Number of nonreadable files = $num" 
