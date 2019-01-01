StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety Four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x

-- Commented, because it breaks the syntax highlighting.
-- valToString2 : (isInt : Bool) -> (case isInt of
--                                        False => String
--                                        True => Int) -> String
-- valToString2 False x = trim x
-- valToString2 True  x = cast x

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType =>
        (numargs : Nat) -> (acc: numType) -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
