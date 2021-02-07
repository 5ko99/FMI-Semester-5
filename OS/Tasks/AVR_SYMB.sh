read file
chars=`wc -c < $file`
lines=`wc -l < $file`
words=`wc -w < $file`
let avrChars=$chars/$lines
let avrWords=$words/$lines
echo "Average chars per line $avrChars"
echo "Average words per line $avrWords"
