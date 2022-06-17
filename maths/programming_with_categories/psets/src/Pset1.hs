{-# LANGUAGE UnicodeSyntax #-}
module Pset1 where

qHeader∷ Int → IO()
qHeader number = do
    putStrLn $ "Solutions for Question " ++ show number ++ ":"
f ∷ Int → Int
f = error "not implemented"
g ∷ Int → Int
g = error "not implemented"
question1∷ Int → IO()
question1 x =  do
    qHeader 1
    putStrLn $ "x = " ++ show x
    let f x = x * x
    let g  = succ
    putStrLn $ "The f(x) = " ++ show (f x)
    putStrLn $ "The g(x) = " ++ show (g x)

    let fCg = f.g
    let gCf = g.f
    putStrLn $ "The f◦g(x) = " ++ show (fCg x)
    putStrLn $ "The g◦f(x) = " ++ show (gCf x)
    putStrLn ""

question2∷ (Show a, Show b) ⇒ a → b -> IO()
question2 objects morphisms = do
    qHeader 2
    putStrLn $ "The objects = " ++ show objects
    putStrLn $ "The morphisms = " ++ show morphisms
    putStrLn "The composition rule = "
    putStrLn ""

print∷IO()
print = do
    question1 2
    question2 [1,2] [[1,1], [1,2], [2,1], [2,2]]
