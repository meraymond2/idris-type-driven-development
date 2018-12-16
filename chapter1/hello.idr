module Main

main : IO ()                    -- declaration
main = putStrLn "Hello world!"  -- definition

-- A function that returns a type
StringOrInt : Bool -> Type
StringOrInt x = case x of
                      True => String -- reversed from book
                      False => Int

-- A function that can return two types,
-- depending on its input
getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
                      True => "77"
                      False => 77

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of
                         True => val
                         False => cast val

-- You can annotate input types with names.
-- The names don’t _have_ to correspond to the parameters, but they probably should.
-- Even when they aren’t used like they are above, they can be helpful.
addTwoNum : (a : Int) -> (b : Int) -> Int
addTwoNum x y = x + y
