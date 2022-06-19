{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Chapter8
  ( mult',
    nat2int',
    int2nat',
    buildSearchTree',
    buildBinaryTree',
    flattenSearchTree',
    flattenBinaryTree',
    binaryTreeLeaves',
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

buildSearchTree' :: [t] -> SearchTree t
buildSearchTree' (l : r : rs) = SearchNode (SearchLeaf l) r (buildSearchTree' rs)

flattenSearchTree' :: SearchTree t -> [t]
flattenSearchTree' (SearchLeaf leaf) = [leaf]
flattenSearchTree' (SearchNode left _ right) = flattenSearchTree' left ++ flattenSearchTree' right

buildBinaryTree' :: [t] -> BinaryTree t
buildBinaryTree' (l : rs) = BinaryNode (BinaryLeaf l) (buildBinaryTree' rs)

flattenBinaryTree' :: BinaryTree t -> [t]
flattenBinaryTree' (BinaryLeaf leaf) = [leaf]
flattenBinaryTree' (BinaryNode left right) = flattenBinaryTree' left ++ flattenBinaryTree' right

binaryTreeLeaves' :: BinaryTree t -> [t]
binaryTreeLeaves' = flattenBinaryTree'

-- balanced' :: BinaryTree a -> Bool
-- balanced' (BinaryLeaf a) = True
{- balanced' (BInaryNode left root right) =  -}
