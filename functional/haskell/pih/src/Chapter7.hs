module Chapter7
  ( Chapter7.mapFilter',
    Chapter7.all',
    Chapter7.any',
    Chapter7.takeWhile',
    Chapter7.dropWhile',
    Chapter7.mapUsingFoldr',
    Chapter7.filterUsingFoldr',
    Chapter7.dec2int',
    Chapter7.curry',
    Chapter7.uncurry',
    Chapter7.chop8UsingUnfold',
    Chapter7.mapUsingUnfold',
    Chapter7.iterate',
    Chapter7.transmit',
    Chapter7.altMap',
    Chapter7.luhnUsingAltMap',
    Chapter7.int2dec',
    Chapter7.numLength',
    Chapter7.findBankCards',
  )
where

import qualified BinaryStringTransmitter as BT
import qualified Chapter4 as C4

mapFilter' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter' f p list = map f (filter p list)

all' :: (a -> Bool) -> [a] -> Bool
all' p list = and (map p list)

any' :: (a -> Bool) -> [a] -> Bool
any' p list = or (map p list)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = [x]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = xs

mapUsingFoldr' :: (a -> b) -> [a] -> [b]
mapUsingFoldr' f = foldr (\x xs -> f x : xs) []

filterUsingFoldr' :: (a -> Bool) -> [a] -> [a]
filterUsingFoldr' p = foldr (\x xs -> if p x then x : xs else xs) []

-- TODO: this only handles list with 1 digit numbers, what about an even more general approach?
dec2int' :: [Int] -> Int
dec2int' = foldl (\place digit -> place * 10 + digit) 0

nums' :: Int -> [Int]
nums' 0 = []
nums' n = n : nums' (n `div` 10)

numLength' :: Int -> Int
numLength' = length . nums'

int2dec' :: Int -> [Int]
int2dec' 0 = []
int2dec' 1 = [1]
int2dec' n
  | n < 0 = error "Negative number handling not implemented"
  | otherwise =
    let nextpower = 10 ^ (numLength' n - 1)
        (q, rem) = n `quotRem` nextpower
     in if numLength' n - numLength' rem > 1
          then q : 0 : int2dec' (n `mod` nextpower)
          else q : int2dec' (n `mod` nextpower)

-- TODO: These only deal with a pair, not tuple
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' fn = \x y -> fn (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' fn = \(x, y) -> fn x y

type Bit = Int

chop8UsingUnfold' :: [Bit] -> [[Bit]]
chop8UsingUnfold' = BT.unfold' (== []) (take 8) (drop 8)

mapUsingUnfold' :: (a -> b) -> [a] -> [b]
mapUsingUnfold' fn = BT.unfold' null (fn . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = BT.unfold' (const False) id

transmit' :: String -> String
transmit' = BT.decode' . BT.channel' . BT.encode'

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' f g [] = []
altMap' f g (x : xs) = f x : altMap' g f xs

luhnUsingAltMap' :: Int -> Bool
luhnUsingAltMap' numbers = sum (altMap' C4.luhnDouble id (int2dec' numbers)) `mod` 10 == 0

findBankCards' :: [Int] -> [Int]
findBankCards' list = [card | card <- list, luhnUsingAltMap' card]
