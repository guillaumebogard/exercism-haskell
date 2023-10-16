module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA ""       = Right ""
toRNA ('G':xs) =
    let rest = toRNA xs
    in case rest of
        l@Left {} -> l
        Right r   -> Right $ 'C' : r
toRNA ('C':xs) =
    let rest = toRNA xs
    in case rest of
        l@Left {} -> l
        Right r   -> Right $ 'G' : r
toRNA ('T':xs) =
    let rest = toRNA xs
    in case rest of
        l@Left {} -> l
        Right r   -> Right $ 'A' : r
toRNA ('A':xs) =
    let rest = toRNA xs
    in case rest of
        l@Left {} -> l
        Right r   -> Right $ 'U' : r
toRNA (x  :_ ) = Left x
