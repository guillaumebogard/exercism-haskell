module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz 1       = Just 0
collatz n
    | n <= 0    = Nothing
    | even n    = (+1) <$> collatz (n `div` 2)
    | otherwise = (+1) <$> collatz (3 * n + 1)
