module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ Set.fromList $ sumOfMultiples' factors limit

sumOfMultiples' :: [Integer] -> Integer -> [Integer]
sumOfMultiples' []     _     = []
sumOfMultiples' (0:_)  _     = [0]
sumOfMultiples' (n:ns) limit = takeWhile (< limit) (map (* n) [1..]) ++ sumOfMultiples' ns limit
