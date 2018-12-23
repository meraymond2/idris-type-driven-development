{- Enumerated Type -}

-- An enumerated type is one where you explicitly list all
-- of the possible values.

data Cat = Cas | Luna | Sherlock

data Direction = North
               | South
               | East
               | West

catToString : Cat -> String
catToString Cas = "cascat"
catToString Luna = "lunabee"
catToString Sherlock = "sherlocked"


{- Union Type -}

-- Like a enum type, but the constructor also carries data.

-- There are two forms of data-type declaration.
-- This is the slightly shorter form.

||| Represents shapes
data Shape = ||| A triangle, given base length and height
             Triangle Double Double
           | ||| A rectangle, given length and height
              Rectangle Double Double
           | ||| A circle, given its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

-- Slightly more verbose, but more flexible
data Shape2 : Type where
  Triangle2 : Double -> Double -> Shape2
  Rectangle2 : Double -> Double -> Shape2
  Circle2 : Double -> Shape2

-- doesn't just apply to Union types
-- eg. for Enum:
data Colour : Type where
  Red : Colour
  Green : Colour
  Blue : Colour


{- Recursive Type -}

-- Recursive types have constructors that use the type itself
-- There must be one non-recursive constructor, so that
-- it has something to reduce to.

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

rect : Picture
rect = Primitive (Rectangle 20 10)

circ : Picture
circ = Primitive (Circle 5)

tri : Picture
tri = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rect)
              (Combine (Translate 35 5 circ)
                       (Translate 15 25 tri))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

{- Generic Type -}

-- A generic data type is one that paramterised over
-- another type

data Biggest = NoTriangle | Size Double

biggestTriangle : Picture -> Biggest
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
biggestTriangle (Primitive tri@(Triangle x y)) = Size (area tri)
biggestTriangle (Primitive (Rectangle x y)) = NoTriangle
biggestTriangle (Primitive (Circle x)) = NoTriangle
biggestTriangle (Combine pic pic1) = compareSizes (biggestTriangle pic) (biggestTriangle pic1) where
                                     compareSizes : Biggest -> Biggest -> Biggest
                                     compareSizes NoTriangle NoTriangle = NoTriangle
                                     compareSizes NoTriangle (Size x) = Size x
                                     compareSizes (Size x) NoTriangle = Size x
                                     compareSizes (Size x) (Size y) = case x > y of
                                                                           False => Size y
                                                                           True => Size x

data DivResult = DivByZero | Result Double
safeDivide : Double -> Double -> DivResult
safeDivide x y = if y == 0 then DivByZero
                           else Result (x / y)

safeDivide2 : Double -> Double -> Maybe Double
safeDivide2 x y = if y == 0 then Nothing else Just (x / y)

-- List is not a type, it's a function
-- that takes a Type and returns another Type
-- in Scala that would be a Kind, but here, it's just a function (!)
-- List Int is a type though

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

-- In order to constrain the type-parameters of the tree
-- you can use the longer notation
data BSTree : Type -> Type where
     BSEmpty : Ord elem => BSTree elem
     BSNode : Ord elem => (left : BSTree elem) -> (val : elem) ->
                        (right : BSTree elem) -> BSTree elem

-- It's easier to see how the constructors are just functions
-- with the latter.

{- exercises -}

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node Empty x right) = x :: treeToList right
treeToList (Node left@(Node tree y z) x right) = treeToList left ++ x :: treeToList right

data Expr = SingleInt Int
          | Addition Expr Expr
          | Subtraction Expr Expr
          | Multiplication Expr Expr

evaluate : Expr -> Int
evaluate (SingleInt i) = i
evaluate (Addition x y) = (evaluate x) + (evaluate y)
evaluate (Subtraction x y) = (evaluate x) - (evaluate y)
evaluate (Multiplication x y) = (evaluate x) * (evaluate y)
