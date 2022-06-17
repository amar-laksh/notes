module Chapter6
  ( Chapter6.sumdown,
    Chapter6.euclid,
    Chapter6.and',
    Chapter6.concat',
    Chapter6.replicate',
    (Chapter6.!!),
    Chapter6.elem',
    Chapter6.merge',
    Chapter6.halve',
    Chapter6.msort',
    Chapter6.take',
    Chapter6.zip',
  )
where

sumdown :: Int -> Int
sumdown 0 = 0
sumdown limit = limit + sumdown (limit - 1)

euclid :: Int -> Int -> Int
euclid a b
  | a == b = a
  | a > b = euclid b (a - b)
  | otherwise = euclid a (b - a)

and' :: [Bool] -> Bool
and' [] = False
and' [_] = True
and' (value : values) = value && and' values

concat' :: [[el]] -> [el]
concat' [] = []
concat' [el] = el
concat' (el : els) = el ++ concat' els

replicate' :: Int -> el -> [el]
replicate' 0 _ = []
replicate' limit value = value : replicate' (limit - 1) value

(!!) :: [el] -> Int -> el
[] !! _ = error "can't select an element from an empty list"
(el : _) !! 0 = el
(_ : els) !! nthElement = els Chapter6.!! (nthElement - 1)

elem' :: Eq el => el -> [el] -> Bool
elem' _ [] = False
elem' value (el : els) = value == el || elem' value els

merge' :: Ord el => [el] -> [el] -> [el]
merge' a [] = a
merge' [] b = b
merge' (a : as) (b : bs)
  | a >= b = b : merge' (a : as) bs
  | otherwise = a : merge' as (b : bs)

halve' :: [el] -> ([el], [el])
halve' [] = ([], [])
halve' list
  | even (length list) = (take (length list `div` 2) list, fst (halve' (reverse list)))
  | otherwise = (head list : fst (halve' (tail list)), snd (halve' (tail list)))

msort' :: Ord el => [el] -> [el]
msort' [] = []
msort' [el] = [el]
msort' list = merge' (msort' (fst (halve' list))) (msort' (snd (halve' list)))

-- Additional Bonus solutions

zip' :: [fst] -> [snd] -> [(fst, snd)]
zip' [] _ = []
zip' _ [] = []
zip' (fh : fs) (sh : ss) = (fh, sh) : zip' fs ss

take' :: Int -> [el] -> [el]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : [v | (_, v) <- zip' [1 .. n - 1] xs]
