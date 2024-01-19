module           Anagram             ( anagramsFor ) where

import qualified Data.Char     as DC ( toLower     )
import qualified Data.MultiSet as DM ( fromList    )

anagramsFor :: String -> [String] -> [String]
anagramsFor word (x:xs)
    | lower_word             == lower_x             =     anagramsFor word xs
    | DM.fromList lower_word == DM.fromList lower_x = x : anagramsFor word xs
    | otherwise                                     =     anagramsFor word xs
    where
        lower_word = map DC.toLower word
        lower_x    = map DC.toLower x
anagramsFor _    []                                 = []
