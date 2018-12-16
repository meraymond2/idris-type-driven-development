module Main

-- 2
palindrome : String -> Bool
palindrome str = str == (reverse str)

-- 3 case insensitive
palindrome2 : String -> Bool
palindrome2 str = let downcased = (toLower str)
                      reversed = (reverse downcased)
                   in downcased == reversed

-- 4 when longer than 10 chars
longPalindrome : String -> Bool
longPalindrome str = let strLength = length str
                         longEnough = strLength > 10
                         isPalindrome = str == (reverse str)
                      in isPalindrome && longEnough

-- 5 longer than x chars
longPalindrome2 : Nat -> String -> Bool
longPalindrome2 n str = let longEnough = (length str) > n
                            palindrome = str == (reverse str)
                         in longEnough && palindrome

-- 6 count words and chars
counts : String -> (Nat, Nat)
counts str = ((length (words str)), length str)

-- 7
top_ten : Ord a => List a -> List a
top_ten list = take 10 (reverse (sort list))

-- 8
over_length : Nat -> List String -> Nat
over_length n list = let wordLengths = map length list
                         keep = filter (\i => i > n) wordLengths
                      in length keep

-- 9 (too lazy to do it for all of them)
printCounts : (Nat, Nat) -> String
printCounts (words, chars) = "There were " ++
                             (cast words) ++
                             " word(s), and " ++
                             (cast chars) ++ " chars in that.\n"

handleInput : String -> String
handleInput input = printCounts(counts input)

main : IO ()
main = repl "Enter a word: " handleInput
