module Bob (responseFor) where

import Data.Text ( Text,
                   pack,
                   unpack
                 )
import Data.Char ( isSpace,
                   isUpper,
                   isLower
                 )

responseFor :: Text -> Text
responseFor x
    | null noSpaces           = pack "Fine. Be that way!"
    | isQuestion && isYelling = pack "Calm down, I know what I'm doing!"
    | isQuestion              = pack "Sure."
    | isYelling               = pack "Whoa, chill out!"
    | otherwise               = pack "Whatever."
    where
        noSpaces              = filter (not . isSpace) $ unpack x
        isQuestion            = last noSpaces == '?'
        isYelling             = any isUpper noSpaces && not (any isLower noSpaces)
