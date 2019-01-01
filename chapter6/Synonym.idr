import Data.Vect

-- This is a type synonym, it's a function that takes
-- no arguments, and returns a type. By convention, it
-- is capitalised, cause it returns a type.
Position : Type
Position = (Double, Double)

Triangle : Type
Triangle = Vect 3 Position

tri : Triangle
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

-- Similar, but with an argument.
Polygon : Nat -> Type
Polygon n = Vect n Position

quad : Polygon 4
quad = [(0.0, 0.0), (3.0, 0.0), (3.0, 4.0), (0.0, 4.0)]

Matrix : Nat -> Nat -> Type
Matrix x y = Vect x (Vect y Double)

TupleVect : Nat -> Type -> Type
TupleVect Z a = ()
TupleVect (S k) a = Pair a (TupleVect k a)

x : TupleVect 0 String
x = ()

testTupleVect : TupleVect 4 Nat
testTupleVect = (1, 2, 3, 4, ())
