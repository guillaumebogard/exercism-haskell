module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map )

data Nucleotide = A | C | G | T
    deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldr nucleotideCountsValidator $ Right Map.empty

nucleotideCountsValidator :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
nucleotideCountsValidator _   l@(Left _)                    = l
nucleotideCountsValidator 'A'   (Right nucleotideCountsMap) = Right $ Map.insertWith (+) A 1 nucleotideCountsMap
nucleotideCountsValidator 'C'   (Right nucleotideCountsMap) = Right $ Map.insertWith (+) C 1 nucleotideCountsMap
nucleotideCountsValidator 'G'   (Right nucleotideCountsMap) = Right $ Map.insertWith (+) G 1 nucleotideCountsMap
nucleotideCountsValidator 'T'   (Right nucleotideCountsMap) = Right $ Map.insertWith (+) T 1 nucleotideCountsMap
nucleotideCountsValidator _     _                           = Left "error"
