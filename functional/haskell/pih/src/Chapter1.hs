module Chapter1
  ( Chapter1.product,
    Chapter1.qsort,
    Chapter1.qsortRev,
  )
where

product :: Num el => [el] -> el
product [] = 1
product (n : ns) = n * Chapter1.product ns

-- Helpers start
upperBound :: Ord el => [el] -> [el]
upperBound [] = []
upperBound (n : ns) = [u | u <- ns, u > n]

lowerBound :: Ord el => [el] -> [el]
lowerBound [] = []
lowerBound (n : ns) = [l | l <- ns, l <= n]

-- Helpers end
--

qsort :: Ord el => [el] -> [el]
qsort [] = []
qsort (n : ns) = qsort (lowerBound (n : ns)) ++ [n] ++ qsort (upperBound (n : ns))

qsortRev :: Ord el => [el] -> [el]
qsortRev [] = []
qsortRev (n : ns) = qsortRev (upperBound (n : ns)) ++ [n] ++ qsortRev (lowerBound (n : ns))
