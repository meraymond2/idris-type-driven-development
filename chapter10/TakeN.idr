data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeNHelp : Nat -> (take : List a) -> (rest : List a) -> (prf : init_list = take ++ rest) -> TakeN init_list
takeNHelp Z take rest prf = rewrite prf in Exact take
takeNHelp (S k) take [] prf = Fewer
takeNHelp (S k) take (x :: xs) prf = ?takeNHelp_rhs_3

appendNilLeftNeutral : (list : List a) -> [] ++ list = list
appendNilLeftNeutral list = Refl

total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z init_list = Fewer
takeN (S k) init_list = takeNHelp (S k) [] init_list (appendNilLeftNeutral init_list)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest
