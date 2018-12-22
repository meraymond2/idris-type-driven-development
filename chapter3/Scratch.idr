module Main

import Data.Vect

-- basic pattern matching of arguments
invert : Bool -> Bool
invert False = True
invert True = False

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Not empty, tail = " ++ show xs

allLengths: List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

||| Inefficient even function.
isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

-- Defining functions in terms of each other...
mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]

sixInts : Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts
