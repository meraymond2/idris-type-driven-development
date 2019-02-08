{-
This wasn't immediately obvious, rec stands for 'recursive'
Snoc is like Successor in (S k). I believe that it can't
be used as a constuctor, but only as a destructor, for
a given SnocList (xs ++ [x]), Snoc (rec) is the xs.
It doesn't record the x, because it doesn't really care about it.
-}
data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelper {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelper {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in
                                                snocListHelper (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelper Empty xs

myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse input = myReverseHelper input (snocList input)


reverseWithoutView : List a -> List a
reverseWithoutView l = helper l [] where
  helper : (xs : List a) -> (ys : List a) -> List a
  helper [] ys = ys
  helper (x :: xs) ys = helper xs (x :: ys)
