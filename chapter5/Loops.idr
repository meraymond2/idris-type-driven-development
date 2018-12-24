module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Done!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs

readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

guess : (target : Nat) -> (guesses : Nat)-> IO ()
guess target guesses = do Just i <- readNum | do putStrLn "Invalid input, guess again."
                                                 guess target (S guesses)
                          case i == target of
                            False => do putStrLn ("Nope. " ++ show guesses ++ " so far.")
                                        guess target (S guesses)
                            True => putStrLn "That's it."

trunc : (millis : Integer) -> Nat
trunc millis = let chars = reverse (unpack (show millis))
                   num_chars = take 2 chars
                in cast (pack num_chars)

main : IO ()
main = do putStrLn "Guess a number: "
          millis <- time
          let n = trunc millis
          guess n 1
