module Chapter9 (
    countdownSolutions',
    countdownNearestSolutions',
    countdownApproxSolutions',
    countdowndownApproxSortedSolutions',
)
where

import qualified CountdownSolver as CS
import Data.List

countdownSolutions' :: [Int] -> Int -> [CS.Expr]
countdownSolutions' = CS.solutions'

countdownNearestSolutions' :: [Int] -> Int -> [CS.Expr]
countdownNearestSolutions' = CS.nearestSolutions'

countdownApproxSolutions' :: [Int] -> Int -> [CS.Expr]
countdownApproxSolutions' = CS.approxSolutions'

countdowndownApproxSortedSolutions' :: [Int] -> Int -> [CS.Expr]
countdowndownApproxSortedSolutions' ns target = do
    let sortedByLength = sort [(length (CS.values' e), e) | e <- countdownApproxSolutions' ns target]
    map snd sortedByLength
