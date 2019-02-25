data InfIO : Type where
  Do : IO a
       -> (a -> Inf InfIO)
       -> InfIO

loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg)
                   (\_ => loopPrint msg)

run : InfIO -> IO()
run (Do action cont) = do res <- action
                          run (cont res)

-- This does the same thing
loopy : IO ()
loopy = do putStrLn "cascat"
           loopy

data Fuel = Dry | More Fuel

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)
--
-- -- Total, but limited
run_ltd : Fuel -> InfIO -> IO()
run_ltd Dry p = putStrLn "Out of fuel"
run_ltd (More fuel) (Do c f) = do res <- c
                                  run_ltd fuel (f res)

data PossiblyInfiniteFuel = Dry | More (Lazy Fuel)

-- You can prove that run can terminate, but
-- you also make it run forever. This is non-total
-- but it means you can keep run total.
forever : PossiblyInfiniteFuel
forever = More forever
