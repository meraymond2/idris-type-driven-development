module Main

import Data.Vect


readVectKnownLen : (len : Nat) -> IO (Vect len String)
readVectKnownLen Z = pure []
readVectKnownLen (S k) = do x <- getLine
                            xs <- readVectKnownLen k
                            pure (x :: xs)

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

-- anyVect : (n ** Vect n String)
-- anyVect = (3 ** ?hole)

{- This is a bit tricky, but you _can't_ just return a vector of
unknown length. If you return a vector, the length must
be defined somewhere previously in the signature. Normally
that is in relation to the other arguments.

This won't type check, cause n isn't the same thing as 0.
repeat : Nat -> Vect n Nat
repeat k = []

This works though: -}
repeat : (n : Nat) -> String -> Vect n String
repeat Z s = []
repeat (S k) s = s :: repeat k s

-- and this
repeatWord : (str : String) -> Vect (length str) String
repeatWord str = repeat (length str) str

range : (n : Nat) -> Vect n Nat
range n = reverse (loop n) where
            loop : (i : Nat) -> Vect i Nat
            loop Z = []
            loop (S k) = k :: loop k


-- So you get around this by return a number _and_ a vect of
-- whatever that number happens to be. In the below, the
-- type checker can apparently infer the lengths.
readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank to end): "
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector (blank to end): "
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different length."
                    Just vec2' => printLn (zip vec1 vec2')

main : IO ()
main = zipInputs
