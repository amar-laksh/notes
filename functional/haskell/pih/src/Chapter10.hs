module Chapter10
  ( putStr',
    adder',
    adderSeq',
    readLine',
    getLine',
    getChar',
  )
where

import Control.Monad (unless)
import Data.Maybe
import System.IO
import Text.Read

putStr' :: String -> IO ()
putStr' str = sequence_ [putChar chr | chr <- str]

getInt :: IO Int
getInt = do
  x <- getLine
  -- Ignoring anything that's not a number
  let ans = fromMaybe 0 (readMaybe x :: Maybe Int)
  return ans

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
  x <- getInt
  xs <- readInts (n - 1)
  return (x : xs)

adder' :: IO ()
adder' = do
  putStrLn "How many numbers? "
  n <- getInt
  xs <- readInts n
  putStr "The total is: "
  print (sum xs)

adderSeq' :: IO ()
adderSeq' = do
  putStrLn "How many numbers? "
  n <- getInt
  xs <- sequence [getInt | x <- [1 .. n]]
  putStr "The total is: "
  print (sum xs)

getChar' :: IO Char
getChar' = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLine' :: IO String
readLine' = getLine' ""

getLine' :: String -> IO String
getLine' xs = do
  x <- getChar'
  case x of
    '\n' -> do
      putChar x
      return xs
    '\DEL' ->
      if null xs
        then getLine' ""
        else do
          putStr "\b \b"
          getLine' (init xs)
    _ -> do
      putChar x
      getLine' (xs ++ [x])
