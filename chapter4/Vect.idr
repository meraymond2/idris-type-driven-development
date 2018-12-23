module MyVect

import Data.Fin
-- A Vect definition. This had initially confused me, because
-- you define the 'sub-types' as constructors.

-- It's both dependent, and recursive
data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

{-
I think that something that confused me in the past about Elm,
which also applies here, is that where you have a data type, the
constructor never applies to the data-type (parent type?). You
can't write Maybe 7, because the Maybe constructor takes a type
to return a type.

I believe that Scala confused me, because it uses X.apply, which
will be a function that returns an instance with a specific type.
So in `val opt: Option[Int] = Option(6)` you have two separate
entities, a type Option[Int] and a function invocation, which is
**not** the type's constructor. The only constructors are still
Nil and Some(_)
-}

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

-- just a quick implementation of index,
-- there's almost certainly a better way to do it
index : (i : Fin n) -> (v : Vect n a) -> a
index {n = (S k)} FZ (x :: xs) = x
index {n = (S k)} (FS j) (y :: ys) = index j ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         Just idx => Just (index idx xs)

testVect : Vect 7 String
testVect = "a" :: "b" :: "c" :: "d" :: "e" :: "f" :: "g" :: Nil
