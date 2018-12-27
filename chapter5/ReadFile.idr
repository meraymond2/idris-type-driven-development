module Main

import Data.Vect

loop : File -> (k ** Vect k String) -> IO (k1 ** Vect k1 String)
loop h (n ** xs) = do end <- fEOF h
                      case end of
                        True => pure (n ** xs)
                        False => do Right x <- fGetLine h | Left _ => loop h (_ ** "error" :: xs)
                                    loop h (_ ** x :: xs)


readVectFile : (fname : String) -> IO (n ** Vect n String)
readVectFile fname = do Right h <- openFile fname Read | Left _ => pure (_ ** [])
                        txt <- loop h (_ ** [])
                        closeFile h
                        pure txt

main : IO ()
main = do fname <- getLine
          (n ** txt) <- readVectFile fname
          putStrLn ("Read file with " ++ (show n) ++ " lines.")
