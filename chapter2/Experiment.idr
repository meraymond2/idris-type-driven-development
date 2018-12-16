module Main

truthy : String -> Bool
truthy str = str == "True"

stringOrBool : Bool -> Type
stringOrBool bool = if bool then Bool else String

takeString : (str : String) -> (stringOrBool (truthy str))
takeString str = ?something

doSomething : Bool -> String
doSomething = ?somethin

-- doSomething : String -> String
-- doSomething = ?something

main : IO ()
main = repl ">> " (\s => (doSomething (takeString s)))
