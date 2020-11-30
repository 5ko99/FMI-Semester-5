until who | grep -q $1
do
   sleep 5
done
echo $1 started a session!!!
