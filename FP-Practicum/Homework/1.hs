dash = "─"

leftAngleUp = "┌"

rightAngleUp = "┐"

leftAngleDown = "└"

rightAngleDown = "┘"

vert = "│ "

vertAfter = " │"

newLine = "\n"

print' 0 _ res = res
print' n f res = print' (n - 1) f f ++ res

p n f = print' n f []

printRowUp n k =
  p (n - k) vert ++ p 1 leftAngleUp
    ++ p ((4 * k) - 3) dash
    ++ p 1 rightAngleUp
    ++ p (n - k) vertAfter
    ++ p 1 newLine

printRowDown n k =
  p (n - k) vert ++ p 1 leftAngleDown
    ++ p (4 * k - 3) dash
    ++ p 1 rightAngleDown
    ++ p (n - k) vertAfter
    ++ p 1 newLine

squares 0 = putStrLn ""
squares n = putStrLn $ helper n n
  where
    helper _ 0 = ""
    helper n k = printRowUp n k ++ helper n (k - 1) ++ printRowDown n k

--testing
main :: IO ()
main = do
  squares 8