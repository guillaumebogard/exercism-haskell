module TwelveDays ( recite ) where

import Data.List  ( foldl' )


recite :: Int -> Int -> [String]
recite start stop
    | not (
        start >= 1    && start <= 12 &&
        stop  >= 1    && stop  <= 12 &&
        start <= stop                   
       )                                = []
    | otherwise                         = [reciteDay day | day <- [start .. stop]]

reciteDay :: Int -> String
reciteDay 1   = "On the " ++ nth 1   ++ " day of Christmas my true love gave to me: " ++ giftByDay 1    ++ "."
reciteDay day = "On the " ++ nth day ++ " day of Christmas my true love gave to me: " ++ formattedGifts ++ "."
    where
        formattedGifts     = fst $ foldl' (accumulator $ length giftsReversedOrder) ("", 0) giftsReversedOrder
        giftsReversedOrder = [giftByDay i | i <- [day, day - 1 .. 1]]

accumulator :: Int -> (String, Int) -> String -> (String, Int)
accumulator _       (_  , 0) x = (x                   , 1    )
accumulator nbElems (acc, i) x
    | i == nbElems - 1         = (acc ++ ", and " ++ x, i + 1)
    | otherwise                = (acc ++ ", "     ++ x, i + 1)

nth :: Int -> String
nth       1  =       "first"
nth       2  =       "second"
nth       3  =       "third"
nth       4  =       "fourth"
nth       5  =       "fifth"
nth       6  =       "sixth"
nth       7  =       "seventh"
nth       8  =       "eighth"
nth       9  =       "ninth"
nth       10 =       "tenth"
nth       11 =       "eleventh"
nth       12 =       "twelfth"
nth       _  = error "nth: Day out of bounds"

giftByDay :: Int -> String
giftByDay 1  =       "a Partridge in a Pear Tree"
giftByDay 2  =       "two Turtle Doves"
giftByDay 3  =       "three French Hens"
giftByDay 4  =       "four Calling Birds"
giftByDay 5  =       "five Gold Rings"
giftByDay 6  =       "six Geese-a-Laying"
giftByDay 7  =       "seven Swans-a-Swimming"
giftByDay 8  =       "eight Maids-a-Milking"
giftByDay 9  =       "nine Ladies Dancing"
giftByDay 10 =       "ten Lords-a-Leaping"
giftByDay 11 =       "eleven Pipers Piping"
giftByDay 12 =       "twelve Drummers Drumming"
giftByDay _  = error "giftByDay: Day out of bounds"
