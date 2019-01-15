
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

data EqNat : (n1 : Nat) -> (n2 : Nat) -> Type where
  Same : (n : Nat) -> EqNat n n

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Maybe (EqNat n1 n2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq  => Just (sameS _ _ eq)

-- I don't think you actually _need_ the sameS function, I'm presuming
-- it's included to demonstrate how the compiler knows already that
-- k and j are the same.
checkEqNat2 : (n1 : Nat) -> (n2 : Nat) -> Maybe (EqNat n1 n2)
checkEqNat2 Z Z = Just (Same 0)
checkEqNat2 (S k) (S j) = checkEqNat2 (S k) (S j)
checkEqNat2 _ _ = Nothing

--using the Refl
checkEqNat3 : (n1 : Nat) -> (n2 : Nat) -> Maybe (n1 = n2)
checkEqNat3 Z Z = Just Refl
checkEqNat3 (S k) (S j) = checkEqNat3 (S k) (S j)
checkEqNat3 _ _ = Nothing

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len v = case checkEqNat2 m len of
                             Nothing => Nothing
                             Just (Same len)  => Just v

{- cong says that for any two values that are equal, they'll
still be equal after a function has been applied -}
same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf
-- cons-ing x onto a vect is just a func, so they're by
-- definition equal

{-oook, don't really get this, but I think the point is,
if you leave the right side as a hole, it displays:
a : Type
y : a
xs : List a
?hole : y :: xs = y :: xs
so I think the point is to demonstrate how the compiler unifies x and y
-}
same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

{- had to look this one up, I don't get it at all.
It's a data type representing the fact that three things are
the same, but since it's not a function, there's no way
to construct it. It can only be stated. It seems tautological to me... -}
data ThreeEq : a -> b -> c -> Type where
  Same3 : ThreeEq t t t

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z Same3 = Same3
