{--tuple (x,y,z...)  :t (1,2,3)
 fst, scn
--}

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib(n - 2)

-- pattern matching на прости типове
recipe "luck" = 10
recipe "skill" = 20
recipe "concentrated power of will" = 15
recipe "pleasure" = 5
recipe "pain" = 50
recipe _ = 0

-- sum of elements
sum2 [] = 0
sum2 (h:t) = h + sum2 t

--is list sorted
isSorted  [] = True
isSorted [_] = True
isSorted (a :b : t) = a <= b && isSorted (b : t)

--vector sum
--addVec v1 v2 = (fst v1 + fst v2,snd v1 + snd v2)
addVec (x1,y1) (x2,y2) = (x1+x2,y1+y2)

--guards
fibGuard n
    | n==0 = 0
    | n==1 = 1
    | otherwise = fibGuard (n - 1) + fibGuard (n - 2)

--
calcBmi m h = m / h ^ 2
category m h
    | bmi < 10 = "a"
    | bmi < 20 = "b"
    | otherwise = "c"
    where 
        bmi = calcBmi m h

--fib tail rec


fib1 n = fibIter 0 1 n
    where
        fibIter a _ 0 = a
        fibIter _ b 1 = b
        fibIter a b cnt = fibIter b (a + b) (cnt - 1)


--let
vecLen :: Floating a => (a, a) -> a
vecLen (x, y) = sqrt (x * x + y * y)

normalized :: Floating b => (b, b) -> (b, b)
normalized v@(x, y) = let l = vecLen v in (x / l, y / l)

a=(1,2)
b=[1,2,3]