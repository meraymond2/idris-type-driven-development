data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelper {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelper {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in
                                                snocListHelper (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelper Empty xs

myReverse : List a -> List a
myReverse input with (snocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc rec) = x :: myReverse xs | rec
  -- myReverse (xs ++ [x]) | (Snoc rec) = x :: myReverse xs -- this also works, but rebuilds the view each time

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input input2 with (snocList input)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xrec) | (Snoc yrec) = if x == y then isSuffix xs ys | xrec | yrec
                                                                   else False
