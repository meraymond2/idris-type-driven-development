
-- You can do partial applications
addNums : Int -> Int -> Int -> Int
addNums x y z = x + y + z

-- :let addSix = addNums 6
-- addSix 1 1 => 8

-- You can have variables in types
identity : ty -> ty
identity x = x
-- Note that vars in type declarations in Idris aren't always
-- type vars (like Scala), they might be _values_!

-- Constrained types
-- This one is built into Idris, but you can define your own
double : Num ty => ty -> ty
double x = x + x

-- Num is an interface
-- Eq is also an interface, which a type can implement
isEqual : Eq ty => ty -> ty -> Bool
isEqual x y = x == y

do_twice : (a -> a) -> a -> a
do_twice f x = f (f x)

-- Higher order functions :)
-- with a previously defined function
quadruple : Num ty => ty -> ty
quadruple x = do_twice double x

-- with an anonymous function
quad2 : Num ty => ty -> ty
quad2 x = do_twice (\n => n * 2) x

-- with a where, using a previously specified type
quad3 : Num ty => ty -> ty
quad3 x = do_twice d2 x
  where d2 : ty -> ty
        d2 n = n * 2

-- with a where, re-specifying type, but fits interface
quad4 : Num ty => ty -> ty
quad4 x = do_twice d2 x
  where d2 : Num a => a -> a
        d2 n = n * 2

{- Functions with documentation
docs can be viewed with :doc functionName -}
||| Add two Ints together
||| @x the first number to add
||| @y the second number to add
addTwoNumbers : (x : Int) -> (y : Int) -> Int
addTwoNumbers x y = x + y
