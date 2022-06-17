{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Chapter8
  ( mult',
    nat2int',
    int2nat',
    buildSearchTree',
    occurs',
  )
where

data Nat = Zero | Succ Nat

nat2int' :: Nat -> Int
nat2int' Zero = 0
nat2int' (Succ n) = 1 + nat2int' n

int2nat' :: Int -> Nat
int2nat' 0 = Zero
int2nat' n = Succ (int2nat' (n - 1))

mult' :: Nat -> Nat -> Nat
mult' a b = int2nat' (nat2int' a * nat2int' b)

data SearchTree a = SearchLeaf a | SearchNode (SearchTree a) a (SearchTree a)

data BinaryTree a = BinaryLeaf a | BinaryNode (BinaryTree a) (BinaryTree a)

occurs' :: Ord a => a -> SearchTree a -> Bool
occurs' node (SearchLeaf leaf) = leaf == node
occurs' node (SearchNode left root right) = case compare node root of
  LT -> occurs' node left
  GT -> occurs' node right
  EQ -> True

buildSearchTree' :: [a] -> SearchTree a
buildSearchTree' (l : r : rs) = SearchNode (SearchLeaf l) r (buildSearchTree' rs)

flattenSearchTree' :: SearchTree a -> [a]
flattenSearchTree' (SearchLeaf a) = [a]
flattenSearchTree' (SearchNode left root right) = flattenSearchTree' left ++ [root] ++ flattenSearchTree' right

leavesCount' :: BinaryTree a -> Int
leavesCount' (BinaryLeaf a) = 1
leavesCount' (BinaryNode left right) = leavesCount' left + leavesCount' right

-- balanced' :: BinaryTree a -> Bool
-- balanced' (BinaryLeaf a) = True
{- balanced' (BInaryNode left root right) =  -}
