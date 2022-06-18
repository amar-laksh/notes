import qualified Chapter1 as C1
import qualified Chapter2 as C2
import qualified Chapter4 as C4
import qualified Chapter5 as C5
import qualified Chapter6 as C6
import qualified Chapter7 as C7
import qualified Chapter8 as C8
import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [chapter1Tests]

chapter1Tests :: TestTree
chapter1Tests =
  testGroup
    "Chapter 1"
    [ SC.testProperty "C1.qsort' == C1.qsort'.reverse" $
        \list -> C1.qsort' (list :: [Int]) == C1.qsort' (reverse list),
      SC.testProperty "C1.qsortRev == reverse.C1.qsort'" $
        \list -> C1.qsortRev (list :: [Int]) == reverse (C1.qsort' list),
      SC.testProperty "C1.product' == product'" $
        \list -> C1.product' (list :: [Int]) == product list
    ]

chapter2Tests :: TestTree
chapter2Tests =
  testGroup
    "Chapter 2"
    [ SC.testProperty "C2.lastUsingNthElement == last" $
        \list -> C2.lastUsingNthElement (list :: [Int]) == last list,
      SC.testProperty "C2.lastUsingHeadReverse == last" $
        \list -> C2.lastUsingHeadReverse (list :: [Int]) == last list,
      SC.testProperty "C2.initUsingTake == init" $
        \list -> C2.initUsingTake (list :: [Int]) == init list,
      SC.testProperty "C2.initUsingTailReverse == init" $
        \list -> C2.initUsingTailReverse (list :: [Int]) == init list
    ]

{- chapter4Tests :: TestTree -}
-- chapter4Tests =
-- testGroup
-- "Chapter 4"
-- [ SC.testProperty "C4.halve' == halve" $
-- \list -> C4.halve' (list :: [Int]) == C7.halve' list
{- ] -}
