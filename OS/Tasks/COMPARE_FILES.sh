read f1
read f2
cmp $f1 $f2 >/dev/null
if [ $? -eq 0 ]
then echo "Files are equal"
else echo "Files are not equal"
fi
