totalLen : List String -> Nat
totalLen xs = foldr (\str, len => length str + len) 0 xs

genericTotalLen : Foldable ty => ty String -> Nat
genericTotalLen xs = foldr (\str, len => length str + len) 0 xs

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node l e r) = let lFold = foldr func acc l
                                    rFold = foldr func lFold r
                                 in func e rFold
