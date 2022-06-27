module CountdownSolver
  ( solutions',
    perms',
    interleave',
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
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp x y = x >= 0 && y >= 0

apply' :: Op -> Int -> Int -> Int
apply' Add x y = x + y
apply' Mul x y = x * y
apply' Sub x y = x - y
apply' Div x y = x `div` y
apply' Exp x y = x ^ y

values' :: Expr -> [Int]
values' (Val n) = [n]
values' (App _ left right) = values' left ++ values' right

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
perms' = foldr (concatMap . interleave') [[]]

choices' :: [a] -> [[a]]
choices' = concatMap perms' . subs'

solution' :: Expr -> [Int] -> Int -> Bool
solution' exp ns target = elem (values' exp) (choices' ns) && eval' exp == [target]

split' :: [a] -> [([a], [a])]
split' [] = []
split' [_] = []
split' (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split' xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

combine' :: Expr -> Expr -> [Expr]
combine' l r = [App op l r | op <- ops]

exprs' :: [Int] -> [Expr]
exprs' [] = []
exprs' [n] = [Val n]
exprs' ns = [e | (ls, rs) <- split' ns, l <- exprs' ls, r <- exprs' rs, e <- combine' l r]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns target =
  [exp | ns' <- choices' ns, exp <- exprs' ns', eval' exp == [target]]
