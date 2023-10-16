module Bob (responseFor) where

import Data.Char ( isSpace,
                   isUpper,
                   isLower
                 )

responseFor :: String -> String
responseFor x
    | null noSpaces = "Fine. Be that way!"
    | isQuestion && isYelling = "Calm down, I know what I'm doing!"
    | isQuestion = "Sure."
    | isYelling = "Whoa, chill out!"
    | otherwise = "Whatever."
    where
        noSpaces = filter (not . isSpace) x
        isQuestion = last noSpaces == '?'
        isYelling = any isUpper noSpaces && not (any isLower noSpaces)
