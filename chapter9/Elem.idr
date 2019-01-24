import Data.Vect

-- Doesn't work, cause you cant guarantee that xs's length is > 0
-- removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
-- removeElem value (x :: xs) = case decEq x value of
--                                   (Yes prf) => xs
--                                   (No contra) => x :: removeElem value xs

oneInVector : Elem 1 [1, 2, 3]
oneInVector = Here

casInVector : Elem "Cascat" ["Sherlock", "Luna", "Cascat"]
casInVector = There (There Here)

removeElem : (val : a) -> (xs : Vect (S n) a) -> (prf : Elem val xs) -> Vect n a
removeElem x (x :: xs) Here = xs
removeElem {n = Z} val (x :: []) (There later) = absurd later
removeElem {n = (S k)} val (x :: xs) (There later) = x :: removeElem val xs later

v : Vect 3 String
v = ["Sherlock", "Luna", "Cascat"]

-- You have to provide a proof to use removeElem
v2 : Vect 2 String
v2 = removeElem "Cascat" v (There (There Here))

-- the implicit auto parameter means Idris tries to fill it in automatically
removeElem_auto : (val : a) -> (xs : Vect (S n) a) ->
                  {auto prf : Elem val xs} -> Vect n a
removeElem_auto val xs {prf}= removeElem val xs prf

v2' : Vect 2 String
v2' = removeElem_auto "Cascat" v

-- all together now
removeElem2 : (val : a) -> (xs : Vect (S n) a) -> {auto prf : Elem val xs} -> Vect n a
removeElem2 val (val :: ys) {prf = Here} = ys
removeElem2 {n = Z} val (y :: []) {prf = (There later)} = absurd later
removeElem2 {n = (S k)} val (y :: ys) {prf = (There later)} = y :: removeElem2 val ys
-- but the simple proof only works if you know that the elem _is_ in there

notInNil : Elem val [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (val = x) -> Void) -> (notThere : Elem val xs -> Void) -> Elem val (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem' : DecEq ty => (val : ty) -> (xs : Vect n ty) -> Dec (Elem val xs)
isElem' val [] = No notInNil
isElem' val (x :: xs) = case decEq val x of
                            Yes Refl  => Yes Here
                            No notHere => case isElem' val xs of
                                              Yes prf     => Yes (There prf)
                                              No notThere => No (notInTail notHere notThere)
