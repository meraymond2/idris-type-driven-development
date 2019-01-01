module Testing

import Data.Vect

-- Why does this typecheck?
incVec : (k ** Vect k Int) -> (k ** Vect k Int)
incVec (n ** xs) = ((S n) ** 7 :: xs)

-- But this doesn’t?
-- incVec2 : Vect k Int -> Vect k Int
-- incVec2 xs = 7 :: xs

-- And this doesn’t
-- incVec3 : { k : Nat } -> (k ** Vect k Int) -> (k ** Vect k Int)
-- incVec3 (n ** xs) = (_ ** 7 :: xs)

-- Finally, why can’t I do this?
-- incVec4 : {k : Nat} -> (k ** Vect k Int) -> ((S k) ** Vect (S k) Int)
-- incVec4 (n ** xs) = (_ ** 7 :: xs)

-- When I can do this?
incVec5 : {k : Nat} -> Vect k Int -> Vect (S k) Int
incVec5 xs = 7 :: xs

incVec6 : {n : Nat} -> (n ** Vect n Int) -> (n ** Vect (S n) Int)
incVec6 (n ** xs) = (n ** 8 :: xs)

incVec6Long : { n : Nat } -> DPair Nat (\n => Vect n Int) -> DPair Nat (\n => Vect (S n) Int)
incVec6Long (n ** xs) = (n ** 7 :: xs)
