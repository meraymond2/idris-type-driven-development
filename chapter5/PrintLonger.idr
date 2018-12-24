module Main

compareLength : String -> String -> Nat
compareLength x y = let xLen = length x
                        yLen = length y
                     in case xLen > yLen of
                         False => yLen
                         True => xLen


withDo : IO ()
withDo = do putStr "First string: "
            first <- getLine
            putStr "Second string: "
            second <- getLine
            let longer = compareLength first second
            putStrLn (show longer)

withoutDo : IO ()
withoutDo = putStr "First string: " >>=
            \_ => getLine >>=
            \first => putStr "Second string: " >>=
            \_ => getLine >>=
            \second => let longer = compareLength first second
                        in putStrLn (show longer)

main : IO ()
main = withoutDo
