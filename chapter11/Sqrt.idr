next : (n : Double) -> (approx : Double) -> Double
next n approx = (approx + (n / approx)) / 2

square_root_approx : (n : Double) -> (approx : Double) -> Stream Double
square_root_approx n approx = approx :: square_root_approx n (next n approx)

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs) =
  case (value * value) < bound of
    True => value
    False => square_root_bound k number bound xs

square_root : (n : Double) -> Double
square_root n = square_root_bound 100 n 0.00000000001 (square_root_approx n n)
