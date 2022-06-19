import qualified Chapter1 as C1
import qualified Data.List as L
import Test.Tasty.Bench
import qualified Test.Tasty.SmallCheck as SC

lst :: [Int]
lst = [1 .. 1000] :: [Int]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "product bench compare"
        [ bcompare "1000 elements" $ bench "C1.product'" $ whnf C1.product' lst,
          bcompare "1000 elements" $ bench "product" $ whnf product lst,
          SC.testProperty "C1.product' == product" $
            \list -> C1.product' (list :: [Int]) == product list
        ],
      bgroup
        "sorting"
        [ bcompare "one" $
            bench "sort1" $ nf C1.qsort' lst,
          bench "one" $ nf C1.qsort' lst,
          bcompare "one" $ bench "sort" $ nf L.sort lst,
          SC.testProperty "C1.qsort' == sort" $
            \list -> C1.qsort' (list :: [Int]) == L.sort list
        ]
    ]
