module CountdownSolver
  ( countdown',
    perms',
    interleave',
    choices',
  )
where

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

data Expr
  = Val Int
  | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App op l r) = bracket l ++ show op ++ bracket r
    where
      bracket (Val v) = show v
      bracket expr = "(" ++ show expr ++ ")"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Mul _ _ = True
valid Sub x y = x > y
valid Div x y = x `mod` y == 0
valid Exp _ y = y > 0

apply' :: Op -> Int -> Int -> Int
apply' Add x y = x + y
apply' Mul x y = x * y
apply' Sub x y = x - y
apply' Div x y = x `div` y
apply' Exp x y = x ^ y

eval' :: Expr -> [Int]
eval' (Val v) = [v | v > 0]
eval' (App op l r) = [apply' op x y | x <- eval' l, y <- eval' r, valid op x y]

subs' :: [a] -> [[a]]
subs' [] = [[]]
subs' (x : xs) = map (x :) yss ++ yss
  where
    yss = subs' xs

interleave' :: a -> [a] -> [[a]]
interleave' n [] = [[n]]
interleave' n (x : xs) = (n : x : xs) : map (x :) (interleave' n xs)

perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' (x : xs) = concatMap (interleave' x) (perms' xs)

choices' :: [a] -> [[a]]
choices' = concatMap perms' . subs'

countdown' :: [Int] -> Int -> Expr
countdown' seq target = Val target
