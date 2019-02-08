import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

mergeSort : Ord a => List a -> List a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)

-- p 279 exercises
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] input2 | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xrec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc xrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xrec) | (Snoc yrec) =
      if x == y then equalSuffix xs ys | xrec | yrec ++ [x]
      else []

-- same as for lists, though I bet splitRec is more complicated
v_mergeSort : Ord a => Vect l a -> Vect l a
v_mergeSort input with (splitRec input)
  v_mergeSort [] | SplitRecNil = []
  v_mergeSort [x] | SplitRecOne = [x]
  v_mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) = merge (v_mergeSort xs | lrec) (v_mergeSort ys | rrec)

toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = (toBinary n | rec) ++ "1"

palindrome : String -> Bool
palindrome input = helper (unpack input) where
  helper : List Char -> Bool
  helper cs with (vList cs)
    helper [] | VNil = True
    helper [x] | VOne = True
    helper (x :: (xs ++ [y])) | (VCons rec) =
      if x == y then helper xs | rec
      else False
