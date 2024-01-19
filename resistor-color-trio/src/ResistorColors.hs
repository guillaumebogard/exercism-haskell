module ResistorColors (Color(..), Resistor(..), label, ohms) where

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

label :: Resistor -> String
label (Resistor (ten, unit, zeros)) = let (value, measurement_unit) = label' ((fromEnum ten * 10 + fromEnum unit) * 10 ^ fromEnum zeros) in value ++ " " ++ measurement_unit

label' :: Int -> (String, String)
label' value
  | value `div` 1000000000 > 1 = (show (value `div` 1000000000), "gigaohms")
  | value `div` 1000000    > 1 = (show (value `div` 1000000   ), "megaohms")
  | value `div` 1000       > 1 = (show (value `div` 1000      ), "kiloohms")
  | otherwise                  = (show  value                  , "ohms"    )

ohms :: Resistor -> Int
ohms (Resistor (ten, unit, zeros)) = (fromEnum ten * 10 + fromEnum unit) * 10 ^ fromEnum zeros
