read file1
read file2
read result
cat $file1 $file2 >temp
sort temp >$result
rm temp
du -h $file1
du -h $file2
du -h $result
