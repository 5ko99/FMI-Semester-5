read file
read str
grep -q $str $file
if [ $? -eq 0 ]
then echo "Found it!"
else echo "It's not in the file"
fi
