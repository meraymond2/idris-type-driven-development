module Main

readFileName : IO String
readFileName = do putStr "Filename: "
                  getLine


readToBlank : IO (List String)
readToBlank = loop [] where
              loop : List String -> IO (List String)
              loop xs = do str <- getLine
                           (case str == "" of
                                 False => loop (str :: xs)
                                 True => pure xs)


readAndSave : IO ()
readAndSave = do txt <- readToBlank
                 fname <- readFileName
                 write <- writeFile fname (unlines (reverse txt))
                 case write of
                       Left err => putStrLn "Failed to write to file."
                       Right () => putStrLn ("Wrote to " ++ fname ++ ".")

main : IO ()
main = readAndSave
