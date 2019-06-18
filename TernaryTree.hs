module TernaryTree
( TernaryTree(..)
, prettyShow ) where

data TernaryTree a = Empty | Tree a (TernaryTree a)
                                    (TernaryTree a)
                                    (TernaryTree a)
  deriving (Show, Read, Eq, Ord)


-- Generates a more readable representation of a tree as a string.
prettyShow :: (Show a) => TernaryTree a -> String
prettyShow t = indentShow 0 t
  where indentShow n Empty = replicate (2 * n) ' ' ++ "Empty"
        indentShow n (Tree x Empty Empty Empty) = replicate (2 * n) ' ' ++ show x ++ " Empty Empty Empty"
        indentShow n (Tree x t1 t2 t3) =  replicate (2 * n) ' ' ++ show x ++ "\n"
                                      ++ indentShow (n + 1) t1 ++ "\n"
                                      ++ indentShow (n + 1) t2 ++ "\n"
                                      ++ indentShow (n + 1) t3