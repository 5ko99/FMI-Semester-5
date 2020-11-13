import Data.Char

main :: IO()
main = do
    putStr $ printLoop 0 1000
    return ()

printLoop a b = helper a ""
    where
        helper i result
            | i > b = result
            | mod i 3 == 0 && mod i 5 == 0 = helper (i + 1) (result ++ "FooBar\n")
            | mod i 3 == 0 = helper (i + 1) (result ++ "Foo\n")
            | mod i 5 == 0 = helper (i + 1) (result ++ "Bar\n")
            | otherwise = helper (i + 1) (result ++ (show i) ++ "\n")


--zad3
member _ [] = False
member x (h:t) =
    (x==h || member x t) 

listUnion [] l = l
listUnion (h:t) l 
    | member h l = listUnion t l
    | otherwise = listUnion t $ h:l

listIntersection l1 l2 = helper l1 l2 []
    where
        helper [] _ res = res
        helper (h:t) l res
            |member h l = helper t l (h:res)
            |otherwise = helper t l res

numToString n 
    |n==0 = ['0']
    |n<0 = ('-':helper (-n) [])
    |otherwise = helper n []
        where
            helper 0 res = res
            helper n res = helper (n `quot` 10) (chr (ord '0'+(n `mod` 10)):res)


stringToNum "" = 0
stringToNum "0" = 0
stringToNum ('-':t) = (-1*stringToNum t)
stringToNum str = 
    let len = length str in helper str len 0
    where
        helper _ 0 res = res
        helper (h:t) len res = helper t (len-1) res + (10^(len-1)*(ord h - ord '0'))


transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

            