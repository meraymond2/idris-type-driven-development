{-
1. If it's empty, return an empty list.
2. If it's one element, it's sorted, return that element.
3. Else, split it into two halves, recursively sort those halves,
then merge the halves back in (?).
-We're cheating, cause there's already a merge func, that will
merge two sorted lists.
-}

--hypothetical mergeSort
-- doesn't work, cause ls is a, not List a
-- mergeSort : Ord a => List a -> List a
-- mergeSort [] = []
-- mergeSort [x] = [x]
-- mergeSort (ls ++ rs) = merge (mergeSort ls) (mergeSort rs)

data SplitList : List a -> Type where
  SplitNil : SplitList []
  SplitOne : SplitList [x]
  SplitPair : (ls : List a) -> (rs : List a) -> SplitList (ls ++ rs)

{- This gives me a bit of a headache, but I think it's because I'm
not thinking in dependent types.
1. So List takes a type and returns a type. List (Int) returns
a Type. The Nil constructor returns a _value_ of the type (List Int).
2. SplitList ([7]) returns a concrete type, the 7 _doesn't matter at all_,
the type only indicates the fact that there's one element. The
SplitOne constructor doesn't know about the [7], it doesn't care.
That [7] comes from the type declaration, _just like Nil doesn't know
what type the list is._
3. The type doesn't hold the value, you can't extract that 7, cause
it's not a value, it's a type, *and you can't pattern match on types*.
4. The 7 isn't recorded in the value, and it only exists in the type in
as far as there's only one of them. The point of the type is that it
tells you that there's one thing in there — and the constructor is how
you match on it. (Constructors seem like they're more important as destructors.)
5. You can sometimes pull in the value of the type in as an implicit argument t
too though.
-}
examp : SplitList [7]
examp = SplitOne -- this doesn't do much on its own


total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelp input input where
  splitListHelp : List a -> (input : List a) -> SplitList input
  splitListHelp xs [] = SplitNil
  splitListHelp xs [x] = SplitOne
  -- counter is just a (confusing) way of counting half-way through the list
  splitListHelp (_ :: _ :: counter) (item :: items)
    = case splitListHelp counter items of
           SplitNil => SplitOne -- if items is [], there's one item left
           SplitOne {x} => SplitPair [item] [x] -- if there's one in items, you have it split one and one
           SplitPair ls rs => SplitPair (item :: ls) rs
  splitListHelp _ items = SplitPair [] items

mergeSort : Ord a => List a -> List a
mergeSort xs with (splitList xs)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (ls ++ rs) | (SplitPair ls rs) = merge (mergeSort ls) (mergeSort rs)
