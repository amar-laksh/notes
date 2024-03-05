module Main where

import AbstractMachine qualified as AM
import Chapter1 qualified as C1

{-import qualified Nim as N-}

import Chapter2 qualified as C2
import Chapter4 qualified as C4
import Chapter5 qualified as C5
import Chapter6 qualified as C6
import Chapter7 qualified as C7
import Chapter8 qualified as C8

{-import qualified Chapter9 as C9-}
import Chapter10 qualified as C10

-- import Chapter11 qualified as C11
-- import Chapter12 qualified as C12
import TautologyChecker qualified as TC

main :: IO ()
main = do
    let lst = [5, 1, 6, 4, 3, 2]
    let sorted = C1.qsort' lst
    let revSorted = C1.qsortRev sorted

    print lst
    print sorted
    print revSorted
    print (C1.product' lst)

    print (C2.lastUsingNthElement revSorted)
    print (C2.lastUsingHeadReverse revSorted)
    print (C2.initUsingTake revSorted)
    print (C2.initUsingTailReverse revSorted)

    print (C4.halve' lst)
    print (C4.thirdUsingHeadTail lst)
    print (C4.thirdUsingListIndexing lst)
    print (C4.thirdUsingPatterMatching lst)
    print (C4.safeTailUsingConditionalExpr [] :: [Int])
    print (C4.safeTailUsingConditionalExpr lst)
    print (C4.safeTailUsingGuardedEquations [] :: [Int])
    print (C4.safeTailUsingGuardedEquations lst)
    print (C4.safeTailUsingPatternMatching [] :: [Int])
    print (C4.safeTailUsingPatternMatching lst)
    print (C4.luhn 4 7 8 3)
    print (C4.luhn 1 7 8 4)

    print (C5.sumOfSquares 100)
    print (C5.grid 1 2)
    print (C5.square 2)
    print (C5.replicate' 3 True)
    print (C5.pyths 10 2)
    print (C5.perfects 500)
    print (C5.find' "blue" [("green", 1), ("blue", 2), ("blue", 3)])
    print (C5.positions' True [False, True, False, True])
    print (C5.positionsUsingFind True [False, True, False, True])
    print (C5.scalarProduct [1, 2, 3] [4, 5, 6])

    print (C6.sumdown 3)
    print (C6.euclid 6 27)
    print (C6.and' [True, True, True])
    print (C6.and' [True, False, True])
    print (C6.concat' [[1, 2, 3], [4, 5, 6]])
    print (C6.replicate' 4 5)
    print (lst C6.!! 3)
    print (C6.elem' 3 [1, 2, 3])
    print (C6.halve' lst)
    print (C6.msort' lst)

    print (C7.all' even [2, 4, 6])
    print (C7.any' even lst)
    print (C7.takeWhile' (< 10) [1 ..])
    print (C7.dropWhile' (< 10) [1 .. 20])
    print (C7.curry' fst 10 20)
    print (C7.uncurry' (*) (3, 2))
    print (C7.mapUsingUnfold' even [0 .. 5])
    print (take 5 (C7.iterate' (* 2) 1))
    print (C7.transmit' "hello functional world")
    print (C7.altMap' (+ 10) (+ 100) [0 .. 4])
    print (C7.luhnUsingAltMap' 1020)
    print (C7.int2dec' 102030)
    print (C7.luhnUsingAltMap' 199)

    let t = [1, 3, 4, 5, 6, 7, 9] :: [Int]
    print (C8.nat2int' (C8.mult' (C8.int2nat' 2) (C8.int2nat' 3)))
    print (C8.listHalves' t)
    print (C8.flattenSearchTree' (C8.buildSearchTree' t))
    print (C8.flattenBinaryTree' (C8.buildBinaryTree' [1, 2, 3, 4]))
    print (C8.binaryTreeLeaves' (C8.buildBinaryTree' [1, 2, 3, 4, 5, 6]))
    print (C8.occurs' 10 (C8.buildSearchTree' [1, 3, 4, 5, 6, 7, 9]))
    print (C8.balanced' (C8.buildBinaryTree' [1 .. 11]))
    print (C8.flattenBinaryTree' (C8.balance' [1, 2, 3, 4]))
    let expr = C8.Add' (C8.Add' (C8.Val' 2) (C8.Val' 4)) (C8.Val' 5)
    print (C8.folde' (+ 2) (*) expr)
    print (C8.eval' expr)
    let prop = TC.And (TC.Var 'A') (TC.Not (TC.Var 'A'))
    print (TC.isTaut' prop)
    print (TC.isTaut' (TC.Imply (TC.And (TC.Var 'A') (TC.Var 'B')) (TC.Var 'A')))
    let expr' = AM.Add (AM.Mult (AM.Val 2) (AM.Val 4)) (AM.Val 5)
    print (AM.value' expr')
    -- print (C9.countdowndownApproxSortedSolutions' [2, 3, 4, 5] 100)

    C10.putStr' "hello world\n"

-- C11.answers
