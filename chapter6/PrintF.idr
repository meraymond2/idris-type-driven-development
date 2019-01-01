data Format = Number Format
            | Str Format
            | Lit String Format
            | Chr Format
            | Dbl Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType (Chr fmt) = (char : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (dbl : Double) -> PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

testString : String
testString = printf "I have %d %s." 2 "cats"

testString2: String
testString2 = printf "ach"


{-
This gave me a major headache. It's not that complicated, but the recurive call isn't
very obvious, so I had difficulty figuring out where the Lit came from.
printf : (fmt : String) -> PrintfType (toFormat ('a' :: 'c' :: 'h'))
toFormat ('a' :: 'c' :: 'h' :: []) =
toFormat [] = End
toFormat 'h' :: [] = Lit (strCons 'h' "") End
toFormat 'c' :: ('h' :: []) = Lit (strCons 'c' "h") End
toFormat 'a' :: ('c' :: 'h' :: []) = Lit (strCons 'a' "ch") End
-}
