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
    listHalves',
    tree',
    balanced',
    balance',
  )
where

import qualified Chapter6 as C6

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

listHalves' :: [t] -> ([t], [t])
listHalves' list = do
  let listLengthHalf = length list `div` 2
  (take listLengthHalf list, reverse (take listLengthHalf (reverse list)))

buildSearchTree' :: [el] -> SearchTree el
buildSearchTree' [el] = SearchLeaf el
buildSearchTree' list = SearchNode (buildSearchTree' (fst (listHalves' list))) (list !! (length list `div` 2)) (buildSearchTree' (snd (listHalves' list)))

flattenSearchTree' :: SearchTree t -> [t]
flattenSearchTree' (SearchLeaf leaf) = [leaf]
flattenSearchTree' (SearchNode left r right) = flattenSearchTree' left ++ [r] ++ flattenSearchTree' right

buildBinaryTree' :: [t] -> BinaryTree t
buildBinaryTree' [el] = BinaryLeaf el
buildBinaryTree' list = BinaryNode (buildBinaryTree' (fst (listHalves' list))) (buildBinaryTree' (snd (listHalves' list)))

tree' :: BinaryTree Int
tree' = BinaryNode (BinaryLeaf 1) (BinaryLeaf 2)

flattenBinaryTree' :: BinaryTree t -> [t]
flattenBinaryTree' (BinaryLeaf leaf) = [leaf]
flattenBinaryTree' (BinaryNode left right) = flattenBinaryTree' left ++ flattenBinaryTree' right

binaryTreeLeaves' :: BinaryTree t -> Int
binaryTreeLeaves' (BinaryLeaf _) = 1
binaryTreeLeaves' (BinaryNode left right) = binaryTreeLeaves' left + binaryTreeLeaves' right

balanced' :: BinaryTree a -> Bool
balanced' (BinaryLeaf a) = True
balanced' (BinaryNode left right)
  | abs (binaryTreeLeaves' left - binaryTreeLeaves' right) <= 1 = True
  | otherwise = False

balance' = buildBinaryTree'
