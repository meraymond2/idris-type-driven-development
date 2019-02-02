-- This doesn't work
-- describeListEnd : List Int -> String
-- describeListEnd [] = "Empty string"
-- describeListEnd (xs ++ [x]) = "Not empty, initial slice = " ++ show xs

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Not empty, initial slice = " ++ show xs

{-
ListLast is a 'view' of lists, because it provides and alternate means of
viewing the data.
-}

-- covering function, converts the type into the view
-- conventially named the same as the view, but lowercase
total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          (NonEmpty ys y) => NonEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

-- with 'with', no need for helper function to inject view
describeListEnd' : List Int -> String
describeListEnd' input with (listLast input)
  describeListEnd' [] | Empty = "Empty"
  describeListEnd' (xs ++ [x]) | (NonEmpty xs x) = "Not empty, initial slice = " ++ show xs

{- case allows you to match on the right side of the equation
   with allows you to match on the left (to extend it) -}

-- Not efficient, (traverses list each time), also not total-checked
-- It can't figure out from the below that ys will converge to []
traverseReverse : List a -> List a
traverseReverse xs with (listLast xs)
  traverseReverse [] | Empty = []
  traverseReverse (ys ++ [y]) | (NonEmpty ys y) = y :: traverseReverse ys
