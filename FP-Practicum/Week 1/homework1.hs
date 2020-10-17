--check for even
even' n = mod n 2 == 0

--factorial
factorial' 1 = 1
factorial' n = n * factorial'(n-1)

--pow
pow x 0 = 1
pow x n = x * pow x (n-1)

--square
square x = x * x

--fast pow
fastPow x 0 = 1
fastPow x n = if mod n 2 == 0 
    then square (fastPow x (n `div` 2))
    else x*square(fastPow x (n `div` 2))

--fib

fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

--tailRec
fib' 1 _last _beforeLast _index = 1
fib' 2 _last _beforeLast _index = 1
fib' n last beforeLast index = if n > index 
    then last
    else fib' (n+1) (last+beforeLast) last index

fib1 n = fib' 3 1 1 n
