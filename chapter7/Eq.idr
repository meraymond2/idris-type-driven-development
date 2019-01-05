occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  False => occurrences item xs
                                  True => 1 + occurrences item xs

-- Implementing Eq for a user defined type
data Cat = Sherlock | Cas | Luna

Eq Cat where
  (==) Sherlock Sherlock = True
  (==) Cas Cas = True
  (==) Luna Luna = True
  (==) _ _ = False
  (/=) x y = not (x == y) -- this is the same as the default definition, so it could be left out

numSherlock : Nat
numSherlock = occurrences Sherlock [Luna, Cas, Sherlock, Cas, Luna]

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

-- This doesn't work, because there's no Eq implementation for elem
-- Eq (Tree elem) where
--   (==) Empty Empty = True
--   (==) (Node l e r) (Node l' e' r') = ?Eq_rhs_2
--   (==) _ _ = False

-- So you need to constrain the type of elem
Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node l e r) (Node l' e' r') = l == l' && e == e' && r == r'
  (==) _ _ = False

{- YOu can only parameterise implementations by
1. data types
2. record types
3. primitive types

You cannot use synonyms, or type functions.
 -}

data Uncomparable = Apples | Oranges

treeOne : Tree Uncomparable
treeOne = Node Empty Oranges Empty

treeTwo : Tree Uncomparable
treeTwo = Node Empty Apples Empty

-- Can't find implementation for Eq (Tree Uncomparable)
-- areEqual : Bool
-- areEqual = treeOne == treeTwo

treeThree : Tree Cat
treeThree = Node Empty Sherlock Empty

treeFour : Tree Cat
treeFour = Node Empty Sherlock Empty

areEqual : Bool
areEqual = treeThree == treeFour

record Cd where
  constructor MkCd
  artist : String
  title : String
  year: Integer

perditionCity : Cd
perditionCity = MkCd "Ulver" "Perdition City" 2000

frysh : Cd
frysh = MkCd "Beyond Dawn" "Frysh" 2003

theTower : Cd
theTower = MkCd "Vulture Industries" "The Tower" 2013

collection : List Cd
collection = [perditionCity, frysh, theTower]

-- Can't find implementatino for Ord Cd
-- sorted : List Cd
-- sorted = sort collection

-- Ord is declared with an Eq constraint, so it kind of
-- extends the Eq interface. To implement Ord, you have
-- to first implement Eq.
Eq Cd where
  (==) (MkCd artist title year) (MkCd artist' title' year')
       = artist == artist' && title == title' && year == year'

-- We only need to implement compare, because everything else
-- is defined in terms of that (and ==)
Ord Cd where
  compare (MkCd artist title year) (MkCd artist' title' year')
          = case compare artist artist' of
                 EQ => (case compare year year' of
                             EQ => compare title title' -- return the string ordering
                             diffYear => diffYear) -- return the int ordering
                 diffArtist => diffArtist -- return the string ordering

sorted : List Cd
sorted = sort collection

-- records don't automatically get show implementations
Show Cd where
  show (MkCd artist title year) = title ++ " by " ++ artist ++ " (" ++ show year ++ ")"


-- Doing the long form, just to get nicely named args
data Shape : Type where
  Triangle : (base : Double) -> (height : Double) -> Shape
  Rectangle : (base : Double) -> (height : Double) -> Shape
  Circle : (radius : Double) -> Shape

{- There is a comment earlier in the book that the compiler
does not accept Eq equality to mean that two things are identical.
This is a good example, because I've decided to implement equality
as meaning the same shape, with the same dimensions. I could just
as easily decide that any Shape with the same area is == though. -}
Eq Shape where
  (==) (Triangle base height) (Triangle base' height') = base == base' && height == height'
  (==) (Rectangle base height) (Rectangle base' height') = base == base' && height == height'
  (==) (Circle radius) (Circle radius') = radius == radius'
  (==) _ _ = False

area : Shape -> Double
area (Triangle base height) = base * height / 2
area (Rectangle base height) = base * height
area (Circle radius) = radius * radius * pi

-- We'll order by area.
Ord Shape where
  compare s s' = compare (area s) (area s')

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
