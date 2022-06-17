module Chapter4
  ( Chapter4.halve,
    Chapter4.thirdUsingHeadTail,
    Chapter4.thirdUsingListIndexing,
    Chapter4.thirdUsingPatterMatching,
    Chapter4.safeTailUsingConditionalExpr,
    Chapter4.safeTailUsingGuardedEquations,
    Chapter4.safeTailUsingPatternMatching,
    Chapter4.luhnDouble,
    Chapter4.luhn,
  )
where

halve :: [el] -> ([el], [el])
halve [] = ([], [])
halve list
  | even (length list) = splitAt (length list `div` 2) list
  | otherwise = error " halve doesn't take odd-sized list"

thirdUsingHeadTail :: [el] -> el
thirdUsingHeadTail list = head (tail (tail list))

thirdUsingListIndexing :: [el] -> el
thirdUsingListIndexing list = list !! 2

thirdUsingPatterMatching :: [el] -> el
thirdUsingPatterMatching [] = error "can't get third element on empty list"
thirdUsingPatterMatching [_, _] = error "can't get third element on list with two elements"
thirdUsingPatterMatching [_] = error "can't get third element on list with one element"
thirdUsingPatterMatching (_ : _ : third : _) = third

null' :: [el] -> Bool
null' [] = True
null' _ = False

safeTailUsingGuardedEquations :: [el] -> [el]
safeTailUsingGuardedEquations list
  | null' list = []
  | otherwise = tail list

safeTailUsingConditionalExpr :: [el] -> [el]
safeTailUsingConditionalExpr list = if null' list then [] else tail list

safeTailUsingPatternMatching :: [el] -> [el]
safeTailUsingPatternMatching [] = []
safeTailUsingPatternMatching (_ : xs) = xs

luhnDouble :: Int -> Int
luhnDouble n
  | n * 2 > 9 = n * 2 - 9
  | otherwise = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn first second trd frt = sum [luhnDouble first, second, luhnDouble trd, frt] `mod` 10 == 0
