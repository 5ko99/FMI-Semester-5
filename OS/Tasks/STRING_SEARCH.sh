read str
for i in $*
do
  echo "$i `grep $str $i | wc -l`"
done
