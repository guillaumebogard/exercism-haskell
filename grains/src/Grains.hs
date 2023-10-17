module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    | n <= 0    = Nothing
    | n > 64    = Nothing
    | otherwise = Just $ 2 ^ (n - 1)

total :: Integer
total = total' 1 1

total' :: Integer -> Integer -> Integer
total' squareIndex totalValue
    | squareIndex >= 64 = totalValue
    | otherwise         = total' (squareIndex + 1) (totalValue + totalValue + 1)
