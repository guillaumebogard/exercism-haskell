module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram sentence = all (checkLetter (map toLower sentence)) ['a' .. 'z']

checkLetter :: String -> Char -> Bool
checkLetter []     _      = False
checkLetter (x:xs) letter
    | x == letter = True
    | otherwise   = checkLetter xs letter
