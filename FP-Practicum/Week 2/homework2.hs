

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
            