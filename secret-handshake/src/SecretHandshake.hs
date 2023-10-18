module SecretHandshake ( handshake ) where

import Data.Bits       ( Bits((.&.)) )

handshake :: Int -> [String]
handshake n
    | n <= 0    = []
    | otherwise = foldr (handshakeValidator n) [] (
            if n .&. (2 ^ 4) /= 0 then
                reverse actions
            else
                actions
            )
    where
        actions = [
                    (2 ^ 0, "wink"),
                    (2 ^ 1, "double blink"),
                    (2 ^ 2, "close your eyes"),
                    (2 ^ 3, "jump")
                ]

handshakeValidator :: Int -> (Int, String) -> [String] -> [String]
handshakeValidator n (key, value) actions
    | key .&. n /= 0 = value : actions
    | otherwise      =         actions
