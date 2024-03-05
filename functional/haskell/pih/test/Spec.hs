import Chapter1 qualified as C1
import Chapter10 qualified as C10
import Chapter12 qualified as C12
import Chapter2 qualified as C2
import Chapter4 qualified as C4
import Chapter5 qualified as C5
import Chapter6 qualified as C6
import Chapter7 qualified as C7
import Chapter8 qualified as C8
import Chapter9 qualified as C9
import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import CountdownSolver qualified as CS
import Data.List
import Data.Maybe
import System.IO.Silently
import Test.SmallCheck.Series (NonEmpty (NonEmpty), NonNegative (NonNegative), list)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck qualified as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [chapter1Tests, chapter2Tests, chapter4Tests, chapter5Tests, chapter6Tests, chapter7Tests, chapter8Tests, chapter9Tests, chapter10Tests, chapter12Tests]

chapter1Tests :: TestTree
chapter1Tests =
    testGroup
        "*** Chapter 1 ***"
        [ SC.testProperty "C1.qsort' == C1.qsort'.reverse"
            $ \list -> C1.qsort' (list :: [Int]) == C1.qsort' (reverse list)
        , SC.testProperty "C1.qsortRev == reverse.C1.qsort'"
            $ \list -> C1.qsortRev (list :: [Int]) == reverse (C1.qsort' list)
        , SC.testProperty "C1.product' == product'"
            $ \list -> C1.product' (list :: [Int]) == product list
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
        "*** Chapter 2 ***"
        [ SC.testProperty "C2.lastUsingNthElement == last"
            $ \(NonEmpty list) -> C2.lastUsingNthElement (list :: [Int]) == last list
        , testCase "C2.lastUsingNthElement [] == error message"
            $ checkErrorMsg C2.errorMsg (C2.lastUsingNthElement [])
        , SC.testProperty "C2.lastUsingHeadReverse == last"
            $ \(NonEmpty list) -> C2.lastUsingHeadReverse (list :: [Int]) == last list
        , testCase "C2.lastUsingHeadReverse [] == error message"
            $ checkErrorMsg C2.errorMsg (C2.lastUsingHeadReverse [])
        , SC.testProperty "C2.initUsingTake == init"
            $ \(NonEmpty list) -> C2.initUsingTake (list :: [Int]) == init list
        , testCase "C2.initUsingTake [] == error message"
            $ checkErrorMsg C2.errorMsg (C2.initUsingTake [])
        , SC.testProperty "C2.initUsingTailReverse == init"
            $ \(NonEmpty list) -> C2.initUsingTailReverse (list :: [Int]) == init list
        , testCase "C2.initUsingTailReverse [] == error message"
            $ checkErrorMsg C2.errorMsg (C2.initUsingTailReverse [])
        ]

chapter4Tests :: TestTree
chapter4Tests =
    testGroup
        "*** Chapter 4 ***"
        [ testCase "C4.halve' [1,2,3,4] == ([1,2], [3,4])"
            $ C4.halve' [1, 2, 3, 4]
            `compare` ([1, 2], [3, 4])
            @?= EQ
        , testCase "C4.halve' [1] == error message"
            $ checkErrorMsg C4.halveErrorMsg (C4.halve' [1])
        , testCase "C4.luhn' 1 2 3 4 == False"
            $ assertBool "Expected this to return False."
            $ not (C4.luhn 1 2 3 4)
        , testCase "C4.luhn' 1 7 8 4 == True"
            $ assertBool "Expected this to return True."
            $ C4.luhn 1 7 8 4
        ]

chapter5Tests :: TestTree
chapter5Tests =
    testGroup
        "*** Chapter 5 ***"
        [ SC.testProperty "C5.scalarProduct is commutative i.e.  a.b = b.a"
            $ \list -> C5.scalarProduct list (take 5 list) == C5.scalarProduct (take 5 list) list
        , SC.testProperty "C5.scalarProduct holds scalar multiplication i.e. (4*a).(2*b) = 8(a.b)"
            $ \list -> C5.scalarProduct (map (* 4) list) (map (* 2) (take 5 list)) == 8 * C5.scalarProduct list (take 5 list)
        , testCase "C5.scalarProduct [1,2,3] [4,5,6] == 32"
            $ C5.scalarProduct [1, 2, 3] [4, 5, 6]
            `compare` 32
            @?= EQ
        ]

chapter6Tests :: TestTree
chapter6Tests =
    testGroup
        "*** Chapter 6 ***"
        [ SC.testProperty "tests C6.halve' by checking length of result lists"
            $ \(list :: [Int]) -> do
                let lengthOfList = length list
                let lengthOfHalfList = length (take (lengthOfList `div` 2) list)
                let lengthOfFstHalve = length (fst (C6.halve' list))
                let lengthOfSndHalve = length (snd (C6.halve' list))
                lengthOfFstHalve - lengthOfHalfList <= 1 && lengthOfSndHalve - lengthOfHalfList <= 1
        , testCase "C6.msort' [5, 4, 0, 3, 2, 1] == [0, 1, 2, 3, 4, 5]"
            $ C6.msort' [5, 4, 0, 3, 2, 1]
            `compare` [0, 1, 2, 3, 4, 5]
            @?= EQ
        ]

chapter7Tests :: TestTree
chapter7Tests =
    testGroup
        "*** Chapter 7 ***"
        [ SC.testProperty "C7.transmit msg == msg"
            $ \(msg :: [Char]) -> C7.transmit' msg == msg
        , testCase "C7.luhnUsingAltMap'  1234 == False"
            $ assertBool "Expected this to return False."
            $ not (C7.luhnUsingAltMap' 1234)
        , testCase "C4.luhnUsingAltMap'   1784 == True"
            $ assertBool "Expected this to return True."
            $ C7.luhnUsingAltMap' 1784
        ]

expr value = C8.Add' (C8.Add' (C8.Val' value) (C8.Val' (value + 1))) (C8.Val' (value + 2))

chapter8Tests :: TestTree
chapter8Tests =
    testGroup
        "*** Chapter 8 ***"
        [ SC.testProperty "tests C8.listHalves' by checking length of result lists"
            $ \(list :: [Int]) -> do
                let lengthOfList = length list
                let lengthOfHalfList = length (take (lengthOfList `div` 2) list)
                let lengthOfFstHalve = length (fst (C8.listHalves' list))
                let lengthOfSndHalve = length (snd (C8.listHalves' list))
                lengthOfFstHalve == lengthOfHalfList && lengthOfSndHalve == lengthOfHalfList
        , SC.testProperty "tests C8.eval' with the expr : (value + (value + 1)) + value + 2"
            $ \(value :: Int) -> C8.eval' (expr value) == ((value + (value + 1)) + value + 2)
        , testCase "C8.occurs' 10 (C8.buildSearchTree' [1, 3, 4, 5, 6, 7, 9]) == False"
            $ assertBool "Expected this to return False."
            $ not (C8.occurs' 10 (C8.buildSearchTree' [1, 3, 4, 5, 6, 7, 9]))
        , testCase "C8.occurs' 9 (C8.buildSearchTree' [1, 3, 4, 5, 6, 7, 9]) == True"
            $ assertBool "Expected this to return True."
            $ C8.occurs' 9 (C8.buildSearchTree' [1, 3, 4, 5, 6, 7, 9])
        , testCase "C8.eval' C8.Add' (C8.Add' (C8.Val' 2) (C8.Val' 4)) (C8.Val' 5) == 11"
            $ assertBool "Expected this to return True." (C8.eval' (C8.Add' (C8.Add' (C8.Val' 2) (C8.Val' 4)) (C8.Val' 5)) == 11)
        ]

chapter9Tests :: TestTree
chapter9Tests =
    testGroup
        "*** Chapter 9 ***"
        [ SC.testProperty "C9.countdownSolutions should yield expressions that eval up to N OR []"
            $ \(list :: [Int], n :: Int) -> do
                ( case map CS.eval' (C9.countdownSolutions' list n) of
                        [] -> True
                        expr -> all (== [n]) expr
                    )
        ]

chapter10Tests :: TestTree
chapter10Tests =
    testGroup
        "*** Chapter 10 ***"
        [ SC.testProperty "C10.putStr' should work the same way as putStr"
            $ \(msg :: String) -> SC.monadic $ do
                (systemPutStr, _) <- capture (putStr msg)
                -- Adding a small thread delay to let capture work correctly
                threadDelay 1000
                (myPutStr, _) <- capture (C10.putStr' msg)
                return $ systemPutStr == systemPutStr
        ]

chapter12Tests :: TestTree
chapter12Tests =
    testGroup
        "*** Chapter 12 ***"
        [ testCase "C12.incBT should increase the Root value"
            $ assertBool "Expected the two resulting trees to be equal" (C12.incBT 1 (C12.Node C12.Leaf 1 C12.Leaf) == C12.Node C12.Leaf 2 C12.Leaf)
        ]
