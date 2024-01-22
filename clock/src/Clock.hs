module Clock ( addDelta
             , fromHourMin
             , toString
             ) where


data Clock = Clock Int Int
  deriving Eq


fromHourMin :: Int -> Int -> Clock
fromHourMin hours minutes = Clock hours_sum minutes_sum
    where
        minutes_carry = minutes     `div` 60
        minutes_sum   = minutes     `mod` 60
        hours_total   = hours + minutes_carry
        hours_sum     = hours_total `mod` 24

toString :: Clock -> String
toString (Clock hours minutes)
    | hours < 10 =
        case () of
          _ | minutes < 10 -> '0' : show hours ++ ':' : '0' : show minutes
            | otherwise    -> '0' : show hours ++ ':' :       show minutes
    | otherwise  =
        case () of
          _ | minutes < 10 ->       show hours ++ ':' : '0' : show minutes
            | otherwise    ->       show hours ++ ':' :       show minutes

addDelta :: Int -> Int -> Clock -> Clock
addDelta hours_to_add minutes_to_add (Clock hours minutes) = fromHourMin (hours + hours_to_add) $ minutes + minutes_to_add
