import World (World (..), getLine', putStrLn')

greet :: IO ()
greet = do
  name <- getLine
  putStrLn $ "hello, " ++ name

greet' :: World -> World
greet' w =
    let(name,wl) = getLine' w in
        putStrLn' ("Hello, " ++ name) wl

branch :: World -> (World, World)
branch w =
    { putStrLn' "Haskell is life" w,
      putStrLn' "Haskell sucks" w
    }

newtype WorldM = World (World ->(a,World))

getLineM :: WorldM String
getLineM = WorldM getLine'

putStrLnM :: String -> WorldM ()
putStrLnM s = WorldM (\w -> ((),putStrLn' s w))

(>>>=) 