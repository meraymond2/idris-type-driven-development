import Data.Vect

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) ->
                   (missing : Vect letters Char) ->
                   WordState guesses_remaining letters

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished
     Won  : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidTwo : ValidInput (x :: x' :: xs) -> Void
isValidTwo (Letter _) impossible

-- So to build this, you need to extract the proofs.
-- Case splitting on the input once extract will return impossible,
-- which is what you want, but not in the body of the main function.
isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput []  = No isValidNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: x' :: xs) = No isValidTwo

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid guess"
                                    readGuess

removeElem : (val : a) -> (xs : Vect (S n) a) -> {auto prf : Elem val xs} -> Vect n a
removeElem val (val :: ys) {prf = Here} = ys
removeElem {n = Z} val (y :: []) {prf = (There later)} = absurd later
removeElem {n = (S k)} val (y :: ys) {prf = (There later)} = y :: removeElem val ys

processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      Yes prf => Right (MkWordState word (removeElem letter missing))
                                                      No contra => Left (MkWordState word missing)

-- Where the hell does guesses come from????
game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter letter) <- readGuess
                                 case processGuess letter st of
                                      Left l => do putStrLn "Wrong."
                                                   case guesses of
                                                        Z => pure (Lost l)
                                                        S k => game l
                                      Right r => do putStrLn "Right."
                                                    case letters of
                                                         Z => pure (Won r)
                                                         S k => game r

-- I only include this to explain the game function above.
-- If its recursive (not necessarily tail-recursive), only
-- the final state has to be the return value, for game Finished,
-- for this example String.
facto : Nat -> String
facto x = loop 1 x where
  loop : (acc : Nat) -> (n : Nat) -> String
  loop acc Z     = (cast acc)
  loop acc (S k) = loop (acc * (S k)) k
