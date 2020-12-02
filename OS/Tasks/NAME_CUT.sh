for curFile in $1/*.c
do
    len=`expr length $curFile`
    let len=${len}-2
    out=`expr substr $curFile 1 ${len}`
    cc -o ${out}.exe $curFile &
done
