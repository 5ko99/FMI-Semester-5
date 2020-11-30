cat >msg
for i in $*
do
if who | grep -q $i
  then
    write $i <msg
  fi
done
