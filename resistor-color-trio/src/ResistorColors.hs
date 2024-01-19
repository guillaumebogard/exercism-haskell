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
label (Resistor (ten, unit, zeros)) = show ((fromEnum ten * 10 + fromEnum unit) * 10 ^ fromEnum zeros) ++ " ohms"

ohms :: Resistor -> Int
ohms resistor = error "You need to implement this function."
