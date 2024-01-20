module           BST             ( BST
                                 , bstLeft
                                 , bstRight
                                 , bstValue
                                 , empty
                                 , fromList
                                 , insert
                                 , singleton
                                 , toList
                                 ) where


import qualified Data.List as DL ( foldl' )


data BST a
    = Empty
    | Node a (BST a) (BST a)
    deriving (Eq, Show)


bstLeft :: BST a -> Maybe (BST a)
bstLeft   Empty                   = Nothing
bstLeft  (Node  _     left _    ) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight  Empty                   = Nothing
bstRight (Node  _     _    right) = Just right

bstValue :: BST a -> Maybe a
bstValue  Empty                   = Nothing
bstValue (Node  value _    _    ) = Just value

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = DL.foldl' (flip insert) Empty

insert :: Ord a => a -> BST a -> BST a
insert x  Empty                   = singleton x
insert x (Node value   Empty   Empty    )
    | x <= value                  = Node value (singleton x  ) Empty
    | otherwise                   = Node value  Empty        $ singleton x
insert x (Node value l@Node {} Empty    )
    | x <= value                  = Node value (insert    x l) Empty
    | otherwise                   = Node value  l            $ singleton x
insert x (Node value   Empty   r@Node {})
    | x <= value                  = Node value (singleton x  ) r
    | otherwise                   = Node value Empty         $ insert    x r
insert x (Node value l@Node {} r@Node {})
    | x <= value                  = Node value (insert    x l) r
    | otherwise                   = Node value  l            $ insert    x r

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList  Empty           = []
toList (Node value l r) = toList l ++ [value] ++ toList r

