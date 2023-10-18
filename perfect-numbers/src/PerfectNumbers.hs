module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify number
    | number <= 0 = Nothing
    | otherwise   = classify' number

classify' :: Int -> Maybe Classification
classify' number
    | aliquotSumOfNumber == number = Just Perfect
    | aliquotSumOfNumber >  number = Just Abundant
    | otherwise                    = Just Deficient
    where
        aliquotSumOfNumber = aliquotSum number

aliquotSum :: Int -> Int
aliquotSum number = sum [x | x <- [number - 1, number - 2 .. 1], number `mod` x == 0]
