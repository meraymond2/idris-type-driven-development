import Data.Primitives.Views

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (n1 :: n2 :: ns) score =
  do putStrLn ("Score so far: " ++ show score)
     putStr (show n1 ++ " * " ++ show n2 ++ "? ")
     answer <- getLine
     if cast answer == n1 * n2
       then do putStrLn "Correct!"
               quiz ns (score + 1)
       else do putStrLn ("Nope, the answer is " ++ show (n1 * n2))
               quiz ns score

-- too random
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223
                in (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound n with (divides n 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

data Face = Heads | Tails

getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf) = if rem == 0
                                            then Heads
                                            else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (x :: xs) = (getFace x) :: (coinFlips k xs)
