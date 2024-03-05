module Chapter12 (
    Chapter12.Tree (..),
    Chapter12.incBT,
)
where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord)

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node l o r) = Node (fmap f l) (f o) (fmap f r)

incBT :: (Functor f, Num n) => n -> f n -> f n
incBT n = fmap (+ n)
