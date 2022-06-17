module BinaryStringTransmitter
  ( encode',
    channel',
    decode',
    unfold',
    addParity',
    checkParity',
  )
where

import Data.Char

type Bit = Int

addParity' :: [Bit] -> [Bit]
addParity' bits
  | even (length (filter (== 0) bits)) = bits ++ [0]
  | otherwise = bits ++ [1]

chop9' :: [Bit] -> [[Bit]]
chop9' = unfold' (== []) (take 9) (drop 9)

checkParity' :: [Bit] -> [Bit]
checkParity' bits
  | addParity' (init bits) == bits = init bits
  | otherwise = error "Parity check failed"

unfold' :: (fn -> Bool) -> (fn -> el) -> (fn -> fn) -> fn -> [el]
unfold' p h t x
  | p x = []
  | otherwise = h x : unfold' p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold' (== 0) (`mod` 2) (`div` 2)

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

make8' :: [Bit] -> [Bit]
make8' bits = take 8 (bits ++ repeat 0)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold' (== []) (take 8) (drop 8)

encode' :: String -> [Bit]
encode' = concatMap (addParity' . make8' . int2bin' . ord)

channel' :: [Bit] -> [Bit]
channel' = id

decode' :: [Bit] -> String
decode' = map (chr . bin2int' . checkParity') . chop9'
