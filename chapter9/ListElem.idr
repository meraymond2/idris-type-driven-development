
data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

oneInList : Elem 1 [1, 2, 3]
oneInList = Here

twoInList : Elem 2 [1, 2, 3]
twoInList = There Here

data Last : List a -> a -> Type where
  LastOne : Last [val] val
  LastCons : (prf : Last xs val) -> Last (x :: xs) val

last123 : Last [1, 2, 3] 3
last123 = LastCons (LastCons LastOne)



notLastInNil : Last [] val -> Void
notLastInNil LastOne impossible
notLastInNil (LastCons _) impossible

lastNotSame : (contra : (x = val) -> Void) -> Last [x] val -> Void
lastNotSame contra LastOne = contra Refl
lastNotSame contra (LastCons prf) = notLastInNil prf

isLast : DecEq a => (xs : List a) -> (val : a) -> Dec (Last xs val)
isLast [] val = No notLastInNil
isLast (x :: []) val = case decEq x val of
                            Yes Refl => Yes LastOne
                            No contra => No (lastNotSame contra)
isLast (x :: xs) val = case isLast xs val of
                            Yes prf => Yes (LastCons prf)
                            No contra => ?yada
