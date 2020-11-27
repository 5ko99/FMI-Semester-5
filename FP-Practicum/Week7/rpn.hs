isOperation :: Char -> Bool
isOperation _ = undefined

operandToFunc op
  | op == '*' = (\a b -> a * b)
  | op == '+' = (\a b -> a + b)
  | op == '-' = (\a b -> a - b)
  | op == '/' = (\a b -> a / b)


