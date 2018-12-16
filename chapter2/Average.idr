module Main

-- Top level functions must have type declarations,
-- but naming types is optional.
average : (str: String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str)) in
                  cast totalLength / cast numWords -- / expects doubles, hence casting
  where -- introduces local function definitions
    wordCount : String -> Nat
    wordCount str = length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map length strs

showAverage : String -> String -- unlike Haskell, String is a primitive type
showAverage str = "The average word length is: " ++
                   show (average str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showAverage
