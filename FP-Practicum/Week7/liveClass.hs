import Control.Monad (forM, forever, when)

mainWhen = do
  line <- getLine

  when (length line > 2) $ do
    putStrLn "in the when 'block'"
    test <- getLine
    putStrLn test
  putStrLn "After when block"

mainSequence = do
  seq <- sequence [getLine, getLine]
  print seq

mainMap = do
  mapM_ print [1, 2, 3] --map for monads

mainMultiplyForever = forever $ do
    a <- getLine
    b <- getLine
    print $ "Ans: " ++ show (read a * read b)