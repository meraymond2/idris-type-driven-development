module Main

my_repl : (prompt : String) -> (onInput : String -> String) -> IO ()
my_repl prompt onInput = do putStr prompt
                            input <- getLine
                            putStrLn (onInput input)
                            my_repl prompt onInput

my_replWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe(String, a)) -> IO ()
my_replWith state prompt onInput = do putStr prompt
                                      input <- getLine
                                      case onInput state input of
                                        Nothing => putStrLn "\n"
                                        Just (str, next) =>
                                          do putStrLn str
                                             my_replWith next prompt onInput

len : String -> String
len x = show (length x)

accLen : (acc : Nat) -> (str : String) -> Nat
accLen acc str = acc + (length str)

handleInput : (acc : Nat) -> (input : String) -> Maybe(String, Nat)
handleInput acc input = let newLen = accLen acc input in
                          case newLen < 100 of
                             False => Nothing
                             True => Just ("New length: " ++ (show newLen), newLen)

main : IO ()
main = my_replWith 0 ">> " handleInput
