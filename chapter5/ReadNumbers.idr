module Main

readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

-- The long version
readNumbers1 : IO (Maybe (Nat, Nat))
readNumbers1 =
  do num1 <- readNum
     case num1 of
          Nothing => pure Nothing
          Just num1_ok =>
               do num2 <- readNum
                  case num2 of
                       Nothing => pure Nothing
                       Just num2_ok => pure (Just (num1_ok, num2_ok))

-- Shorter, but _partial_
readNumbers2 : IO (Maybe (Nat, Nat))
readNumbers2 =
  do Just num1_ok <- readNum
     Just num2_ok <- readNum
     pure (Just (num1_ok, num2_ok))

-- Concise exhaustive pattern matching
readNumbers3 : IO (Maybe (Nat, Nat))
readNumbers3 =
  do Just num1_ok <- readNum | Nothing => pure Nothing
     Just num2_ok <- readNum | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))


main : IO ()
main = do pair <- readNumbers3
          putStrLn (show pair)
