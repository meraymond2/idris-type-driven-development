
-- without stream


label_1_helper : (xs : List a) -> (counter: Integer) -> List (Integer, a)
label_1_helper [] counter = []
label_1_helper (x :: xs) counter = (counter, x) :: label_1_helper xs (counter + 1)

label_1 : List a -> List (Integer, a)
label_1 xs = label_1_helper xs 0

-- Inf marks as potentionally infinite
-- no Nil constructor

{-
You need to mark the second param as Inf, because that's what you're
case splitting on, and that's what Idris would normally check for termination
-}
data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

-- Delay makes it lazy, you can skip it, because if the compiler
-- sees Inf, it'll take the Delay as implicit
countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z (value :: xs) = []
getPrefix (S k) (value :: xs) = value :: (getPrefix k xs)

label_with : Stream labelType -> List a -> List (labelType, a)
label_with (x :: xs) [] = []
label_with (x :: xs) (y :: ys) = (x, y) :: label_with xs ys

label : List a -> List (Integer, a)
label xs = label_with (iterate (+ 1) 0) xs

every_other : Stream a -> Stream a
every_other (x1 :: x2 :: xs) = x2 :: every_other xs

Functor InfList where
  map func (value :: xs) = (func value) :: map func xs
