module Chapter5 (
    sumOfSquares,
    grid,
    square,
    replicate',
    pyths,
    perfects,
    find',
    positions',
    positionsUsingFind,
    scalarProduct,
)
where

sumOfSquares :: (Num n, Enum n) => n -> n
sumOfSquares limit = sum [el ^ 2 | el <- [0 .. limit]]

grid :: Int -> Int -> [(Int, Int)]
grid sizeX sizeY = [(x, y) | x <- [0 .. sizeX], y <- [0 .. sizeY]]

square :: Int -> [(Int, Int)]
square size = [(x, y) | (x, y) <- grid size size, x /= y]

replicate' :: Int -> el -> [el]
replicate' times el = [el | _ <- [1 .. times]]

pyths :: Int -> Int -> [(Int, Int, Int)]
pyths limit power =
    [ (x, y, z)
    | x <- [1 .. limit]
    , y <- [1 .. limit]
    , z <- [1 .. limit]
    , x ^ power + y ^ power == z ^ power
    ]

factors' :: Int -> [Int]
factors' limit = [n | n <- [1 .. limit], limit `mod` n == 0]

perfects :: Int -> [Int]
perfects limit = [n | n <- [1 .. limit], n == sum (factors' n) - n]

find' :: Eq key => key -> [(key, value)] -> [value]
find' key table = [value | (key', value) <- table, key' == key]

positions' :: Eq element => element -> [element] -> [Int]
positions' key list = [position | (position, element) <- zip [0 ..] list, element == key]

positionsUsingFind :: Eq element => element -> [element] -> [Int]
positionsUsingFind key list = find' key (zip list [0 ..])

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct first second = sum [f * s | (f, s) <- zip first second]
