module ReverseString (reverseString) where

reverseString :: String -> String
reverseString (x:xs) = reverseString xs ++ [x]
reverseString []     = []
