module Chapter11
  ( answers,
  )
where

import Data.List (sortOn)
import System.Random
import TicTacToe as T

playTicTacToe = T.tictactoe

countNodes :: GameTree t -> Int
countNodes (Node n []) = 1
countNodes (Node n ts) = 1 + sum [countNodes (Node node trees) | (Node node trees) <- ts]

maxTreeDepth :: GameTree t -> Int
maxTreeDepth (Node n []) = 0
maxTreeDepth (Node n ts) = 1 + maximum (map maxTreeDepth ts)

randomMove' :: [SymbolsGrid] -> SymbolsGrid
randomMove' grid = do
  let randomIdx = fst (randomR (0, length grid - 1) (mkStdGen 2021))
  grid !! randomIdx

randomMove :: Strategy
randomMove grid symbol = do
  bestMoves
  where
    defaultChoiceGrid = [if s == B then [symbol] else [s] | row <- grid, s <- row]
    bestMoves = randomMove' [node | Node (node, symbol') _ <- trees, symbol' == best]
    tree = prune T.treeDepth (gametree grid symbol)
    Node (_, best) trees = T.minimax tree

quickestRouteToWin :: Strategy
quickestRouteToWin grid symbol = do
  bestMoves
  where
    defaultChoiceGrid = [if s == B then [symbol] else [s] | row <- grid, s <- row]
    bestMoves = head [node | Node (node, symbol') _ <- trees, symbol' == best]
    trees = sortOn maxTreeDepth ts
    Node (_, best) ts = T.minimax tree
    tree = prune T.treeDepth (gametree grid symbol)

chooseFirstPlayer :: String -> Symbol
chooseFirstPlayer choice
  | choice == "O" = O
  | choice == "X" = X
  | otherwise = X

answers :: IO ()
answers = do
  print "*** Chapter 11 exercises ***"
  -- T.tictactoe stratergy
  let trees = T.gametree [[B, B, B], [B, B, B], [B, B, B]] X
  -- print (countNodes trees)
  -- print (maxTreeDepth trees)
  -- T.tictactoe randomMove
  print "choose your player:"
  choice <- getLine
  T.tictactoe T.bestmove (chooseFirstPlayer choice) Human T.wins
