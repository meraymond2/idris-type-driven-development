infix 5 //

data Rat : Type where
  (//) : (num : Integer) -> (denom : Integer) -> Rat

half : Rat
half = 1 // 2

oneThird : Rat
oneThird = 1 // 3


divByGCD : Nat -> Nat -> (Nat, Nat)
divByGCD (S a) (S b) = case gcd (S a) (S b) of
                            Z => ((S a), (S b))
                            (S d) => let a' = divNatNZ (S a) (S d) SIsNotZ
                                         b' = divNatNZ (S b) (S d) SIsNotZ
                                      in (a', b')
divByGCD a b = (a, b)

simplify : Rat -> Rat
simplify (num // denom) = let natNum = fromIntegerNat (abs num)
                              natDenom = fromIntegerNat (abs denom)
                              (natNum', natDenom') = divByGCD natNum natDenom
                              intNum' = toIntegerNat natNum'
                              intDenom' = toIntegerNat natDenom'
                              numWasNeg = num < 0
                              denomWasNeg = denom < 0
                           in if numWasNeg == denomWasNeg
                              then intNum' // intDenom'
                              else (- intNum') // intDenom'

-- These are broken if you have zero in the denominator
Num Rat where
  (+) (num // denom) (num' // denom') = simplify ((num * denom' + denom * num') // denom * denom')
  (*) (num // denom) (num' // denom') = simplify ((num * num') // (denom * denom'))
  fromInteger x = x // 1

Neg Rat where
  negate (num // denom) = simplify ((-num) // denom)
  (-) x y = x + (negate y)

Show Rat where
  show (num // denom) = show num ++ "/" ++ show denom

Eq Rat where
  (==) (n1 // d1) (n2 // d2) = let (n1' // d1') = simplify (n1 // d1)
                                   (n2' // d2') = simplify (n2 // d2)
                                in n1' == n2' && d1' == d2'

Cast Rat Double where
  cast (num // denom) = let num' = prim__toFloatBigInt num
                            denom' = prim__toFloatBigInt denom
                         in num' / denom'
