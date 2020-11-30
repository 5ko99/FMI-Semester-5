if test -d $1
then
  for name in $1/*
  do
    if test -f $name -a `wc -c <$name` -gt $2
    then  
      echo $name
    fi
  done
else
  echo There is no such directory
fi
