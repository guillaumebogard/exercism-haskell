module Phone     ( number  ) where

import Data.Char ( isDigit,
                   digitToInt
                 )

data Token = Plus
           | OpenedParenthesis
           | ClosedParenthesis
           | Hyphen
           | Dot
           | Elem              Char

number :: String -> Maybe String
number = parse . tokenize

tokenize :: String -> [Token]
tokenize []       = []
tokenize ('+':xs) = Plus                : tokenize xs
tokenize ('-':xs) = Hyphen              : tokenize xs
tokenize ('.':xs) = Dot                 : tokenize xs
tokenize ('(':xs) = OpenedParenthesis   : tokenize xs
tokenize (')':xs) = ClosedParenthesis   : tokenize xs
tokenize (' ':xs) =                       tokenize xs
tokenize (x  :xs) = Elem              x : tokenize xs

parse :: [Token] -> Maybe String
parse [Plus, Elem '1', OpenedParenthesis, Elem a, Elem b, Elem c, ClosedParenthesis, Hyphen, Elem d, Elem e, Elem f, Hyphen, Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j -- +1 (123)-456-7890
parse [Plus, Elem '1', OpenedParenthesis, Elem a, Elem b, Elem c, ClosedParenthesis,         Elem d, Elem e, Elem f, Hyphen, Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j -- +1 (123) 456-7890
parse [      Elem '1', OpenedParenthesis, Elem a, Elem b, Elem c, ClosedParenthesis,         Elem d, Elem e, Elem f, Hyphen, Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j --  1 (123) 456-7890
parse [                OpenedParenthesis, Elem a, Elem b, Elem c, ClosedParenthesis,         Elem d, Elem e, Elem f, Hyphen, Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j --    (123) 456-7890
parse [                                   Elem a, Elem b, Elem c,                    Dot   , Elem d, Elem e, Elem f, Dot   , Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j --      123.456.7890
parse [                                   Elem a, Elem b, Elem c,                    Hyphen, Elem d, Elem e, Elem f, Hyphen, Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j --      123-456-7890
parse [                                   Elem a, Elem b, Elem c,                            Elem d, Elem e, Elem f,         Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j --      123 456 7890
parse [      Elem '1',                    Elem a, Elem b, Elem c,                            Elem d, Elem e, Elem f,         Elem g, Elem h, Elem i, Elem j] = parseNumberFromChars a b c d e f g h i j --  1   123 456 7890
parse _                                                                                                                                                      = Nothing

parseNumberFromChars :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Maybe String
parseNumberFromChars a b c d e f g h i j
    | not $ all isDigit phoneNumber                      = Nothing
    | digitToInt a < 2 || digitToInt d < 2               = Nothing
    | otherwise                                          = Just    phoneNumber
    where
        phoneNumber = [a, b, c, d, e, f, g, h, i, j]
