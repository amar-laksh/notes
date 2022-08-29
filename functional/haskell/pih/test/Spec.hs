import qualified Chapter1 as C1
import qualified Chapter2 as C2
import qualified Chapter4 as C4
import qualified Chapter5 as C5
import qualified Chapter6 as C6
import qualified Chapter7 as C7
import qualified Chapter8 as C8
import Control.Exception
import Test.SmallCheck.Series (NonEmpty (NonEmpty), list)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [chapter1Tests, chapter2Tests, chapter4Tests]

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

errorPrefix = "This should've thrown the expected error: "

throws :: String -> a -> IO Bool
throws msg val =
  (False <$ evaluate val)
    `catch` \(ErrorCall s) -> pure (s == msg)

checkErrorMsg errorMsg f = throws errorMsg f @? errorPrefix ++ errorMsg

chapter2Tests :: TestTree
chapter2Tests =
  testGroup
    "Chapter 2"
    [ SC.testProperty "C2.lastUsingNthElement == last" $
        \(NonEmpty list) -> C2.lastUsingNthElement (list :: [Int]) == last list,
      testCase "C2.lastUsingNthElement []== error message" $
        checkErrorMsg C2.errorMsg (C2.lastUsingNthElement []),
      SC.testProperty "C2.lastUsingHeadReverse == last" $
        \(NonEmpty list) -> C2.lastUsingHeadReverse (list :: [Int]) == last list,
      testCase "C2.lastUsingHeadReverse []== error message" $
        checkErrorMsg C2.errorMsg (C2.lastUsingHeadReverse []),
      SC.testProperty "C2.initUsingTake == init" $
        \(NonEmpty list) -> C2.initUsingTake (list :: [Int]) == init list,
      testCase "C2.initUsingTake []== error message" $
        checkErrorMsg C2.errorMsg (C2.initUsingTake []),
      SC.testProperty "C2.initUsingTailReverse == init" $
        \(NonEmpty list) -> C2.initUsingTailReverse (list :: [Int]) == init list,
      testCase "C2.initUsingTailReverse []== error message" $
        checkErrorMsg C2.errorMsg (C2.initUsingTailReverse [])
    ]

onlyEven :: ([a] -> Bool) -> [a] -> Bool
onlyEven _ [] = True
onlyEven _ [x] = False
onlyEven _ [x, y] = True
onlyEven f (x : y : xs) = onlyEven f xs

chapter4Tests :: TestTree
chapter4Tests =
  testGroup
    "Chapter 4"
    [ testCase "C4.halve' [1,2,3,4]== ([1,2], [3,4])" $
        C4.halve' [1, 2, 3, 4] `compare` ([1, 2], [3, 4]) @?= EQ,
      testCase "C4.halve' [1] == error message" $
        checkErrorMsg C4.halveErrorMsg (C4.halve' [1])
    ]
