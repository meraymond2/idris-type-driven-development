%default total

data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
             then do putStrLn "Exiting..."
                     Quit ()
             else do putStrLn ("Hello " ++ name)
                     greet

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run Dry p = pure Nothing
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do run forever greet
          pure ()
