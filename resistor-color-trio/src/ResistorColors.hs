module           ResistorColors  ( Color(..),
                                   Resistor(..),
                                   label,
                                   ohms
                                 ) where

import qualified Data.Text as DT ( Text,
                                   pack
                                 )


data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> DT.Text
label resistor =
  let (value, measurement_unit) = (formatOhms . ohms) resistor
    in DT.pack $ show value ++ " " ++ measurement_unit

formatOhms :: Int -> (Int, String)
formatOhms value
  | value `div` 1000000000 > 1 = (value `div` 1000000000, "gigaohms")
  | value `div` 1000000    > 1 = (value `div` 1000000   , "megaohms")
  | value `div` 1000       > 1 = (value `div` 1000      , "kiloohms")
  | otherwise                  = (value                 , "ohms"    )

ohms :: Resistor -> Int
ohms (Resistor (ten, unit, zeros)) = (fromEnum ten * 10 + fromEnum unit) * 10 ^ fromEnum zeros
