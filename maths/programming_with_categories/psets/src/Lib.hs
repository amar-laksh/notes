module Lib (main) where

import Pset1
import Data.List

{- categoryChecker objects morphisms = do -}
    -- putStrLn $ "The objects = " ++ show objects
    -- putStrLn $ "The morphisms = " ++ show morphisms
{-  -}

psetHeader :: Int-> IO()
psetHeader number = do
    putStrLn "=============================="
    putStrLn $ "Problem Set " ++ show number
    putStrLn "=============================="

main :: IO ()
main = do
    psetHeader 1
    Pset1.print
