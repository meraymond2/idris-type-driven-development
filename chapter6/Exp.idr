{- This doesn't hold data, it just holds a type. It's completely useless though,
because I can't instantiate those types without data. -}

data BrokenStrOrInt = String | Int

-- This works, because it's a type synonym
syn : Type
syn = String

-- But this fails, cause I'm trying to _instantiate_ BrokenStrOrInt, and an
-- instance isn't the same as a synonym, and you can't instantiate String like that.
-- test : StrOrInt
-- test = String

-- This is perfectly fine, cause Luna is an Enum, it doesn't need data to
-- construct it, unlike String or Int.
data Cat = Cascat | Luna | Sherlock
cat : Cat
cat = Luna

data StrOrInt = Str String | Inte Int

one : StrOrInt
one = Inte 1

str : StrOrInt
str = Str "string"

-- You cannot match on type, but you can wrap it. This puts the onus of making
-- sure the type is known on you.
printType : StrOrInt -> String
printType (Str _) = "That's a string."
printType (Inte _) = "That's an integer."

one_type : String
one_type = printType one

-- If you have vars in your constructor, that's a type variable.
data Holder = Hold x

-- I don't know what you can do with that though, cause it could be anything.
printHolder : Holder -> String
printHolder (Hold y) = "I can't print y, cause I don't know what it is!"-- ++ (show y)

-- So you could have different constructors for each type you want to hold, or
-- parameterise the type.

data Halter ty = Halt ty

-- This is a bit better, cause now I can access the type in functions.
printHalter : Show ty => Halter ty -> String
printHalter (Halt x) = show x

-- This is the long form of Halter
data Halder : Type -> Type where
  Hald : ty -> Halder ty

-- What if I want it to _only_ be constructable with showable types?

data Hilder : Type -> Type where
  Hild : Show ty => ty -> Hilder ty

printHilder : Hilder t -> String
printHilder (Hild x) = show x

{- I think one of the confusing things is that sometimes you're talking about
types, and in other places, you're talking about data of that type.
The short form is particularly confusing.
Hulder is a function, that accepts a type and returns a type. For example, if it
took a Cat, it would return the type Hulder Cat.
Huld is another function, that accepts data of type ty, and returns an instance
of the type Hulder ty. That instance also contains the data.
It's a bit clearer in the long form, when you can see how they're both functions.
-}
data Hulder ty = Huld ty

thing : Hulder String
thing = Huld "data!"

hulderCat : Hulder Cat
hulderCat = Huld Sherlock

printStringHulder : Hulder String -> String
printStringHulder (Huld s) = s

-- A simple example, showing how the constructors are functions.
data IntHolder = HoldInt i

heldInt : IntHolder
heldInt = HoldInt 100

data DblHolder : Type where
  HoldDbl : Double -> DblHolder

heldDbl : DblHolder
heldDbl = HoldDbl 100.00

{- I think this is still slightly alien to me, because I'm not used to
defining constructors without named arguments.
The below is equivalent to something like:

class Point(x: Double, y: Double) {...}
object Point {
  def apply(x: Double, y: Double): Point = ...
}

But it's the same with regular functions. In dynamic languages, you only name the
parameters, and in some statically typed languages, like Scala, you would do both,
but in the Haskelly ones, you don't name the vars in the signature, only the
implementation. (Obviously in Idris you can label them in the sig if you want to,
but it doesn't affect the implementation, unless you explicitly tell it to.)

-}
data Point = MkPoint Double Double

center : Point
center = MkPoint 0.0 0.0

printPoint : Point -> String
printPoint (MkPoint x y) = (show x) ++ ", " ++ (show y)

{- So, having said the above, you can name your constructor parameters. I appreciate
that the short form is concise, which is fine for the simple cases. The nice thing
about naming your constructor params, is that it automatically names the fields
when you deconstruct them with ctrl/alt+shift+a ! -}

data VerbosePoint : Type where
  MkVerbosePoint : (x_coord : Double) -> (y_coord: Double) -> VerbosePoint

printVerbosePoint : VerbosePoint -> String
printVerbosePoint (MkVerbosePoint x_coord y_coord) = ?printVerbosePoint_rhs_1

{- I have one last thing I want to play with, namely where does the data go
when the thing has been constructed. In a verbose OO language, you'd have to
save the argument to an instance variable. In a Scala case class, you trust that
it's done that for you, and the Haskelly langs seem to do something similar. Can
you implement a custom constructor?

I don't think so, I think that would only make sense for a OO class, whereas
H-langs have algebraic data types.
-}


-- I think it doesn't help that constructors are often named as if they are
-- sub-types, with capital letters, like:
data NumType = IntNum Int
             | DblNum Double

anInt : NumType
anInt = IntNum 7

-- But you could also have lowercase constructors
data NumType2 = intNum Int
              | dblNum Double

aDbl : NumType2
aDbl = dblNum 8.9

-- And infix are just different ways of writing funcs
-- which can be applied to constructors as well
infix 5 <->
data NumPair = NPair Int Double
             | (<->) Int Double

printNumPair : NumPair -> String
printNumPair (NPair x y) = ?printNumPair_rhs_1
printNumPair (x <-> y) = ?printNumPair_rhs_2

newNumPair : NumPair
newNumPair = 1 <-> 1.0
