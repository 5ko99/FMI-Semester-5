read file
read m
read n
let from=$n-$m
to=$n
head -$to $file | tail +$from | sort -r
