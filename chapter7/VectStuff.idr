
-- Vect definition 'borrowed' from Data.Vect
infixr 7 ::
data Vect : (len : Nat) -> (elem : Type) -> Type where
  Nil  : Vect Z elem
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

-- The Eq interface is parameterised by a single type, which means that I
-- have to provide the full (Vect len elem).
Eq elem => Eq (Vect len elem) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

-- Foldable's parameter is Type -> Type, so I provide (Vect len) which is
-- a partially applied type constructor.
Foldable (Vect len) where
  foldr func acc [] = acc
  foldr func acc (x :: xs) = func x (foldr func acc xs)
