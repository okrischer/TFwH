import Data.Char (isAlpha, toLower)

isPalindrome :: String -> Bool
isPalindrome str = test == palindrome
  where
    test = clean str
    clean = map toLower . filter isAlpha
    palindrome = reverse test

check :: String -> String
check line 
  | isPalindrome line = "Yes: " ++ line ++ "\n"
  | otherwise = "No: " ++ line ++ "\n"


main :: IO()
main = interact palindrome
  where palindrome input = concat $ map check $ lines input
