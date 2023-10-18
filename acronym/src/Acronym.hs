
module           Acronym        ( abbreviate ) where

import qualified GHC.Unicode as GHCU

import qualified Data.Text   as T
import           Data.Text      ( Text    )
import           Data.Char      ( isSpace )


abbreviate :: Text -> Text
abbreviate text = snd $ T.foldl abbreviateValidator (Nothing, T.empty) $ T.replace (T.pack "-") (T.pack " ") text

abbreviateValidator :: (Maybe Char, Text) -> Char -> (Maybe Char, Text)
abbreviateValidator (Nothing, abbreviation) c
    | GHCU.isAlpha c = (Just c, T.snoc abbreviation $ GHCU.toUpper c)
    | otherwise      = (Nothing, abbreviation)
abbreviateValidator (Just prev, abbreviation) c
    | GHCU.isAlpha c &&
        ((GHCU.isUpper c && ((not . GHCU.isAlpha) prev || (not . GHCU.isUpper) prev)) || isSpace prev)
                     = (Just c, T.snoc abbreviation $ GHCU.toUpper c)
    | otherwise      = (Just c, abbreviation)
