module Helper where

toEitherList :: [Either err val] -> Either err [val]
toEitherList [] = Right []
toEitherList (Right x : xs) = do
  tail <- toEitherList xs
  return $ x : tail
toEitherList (Left e : _) = Left e