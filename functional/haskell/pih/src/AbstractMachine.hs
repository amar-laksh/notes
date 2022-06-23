module AbstractMachine
  ( Expr (..),
    value',
  )
where

data Expr
  = Val Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show, Eq)

data Op = EVALA Expr | EVALM Expr | ADD Int | MULT Int

type Cont = [Op]

exec' :: Cont -> Int -> Int
exec' [] v = v
exec' (EVALA x : c) y = eval' x (ADD y : c)
exec' (EVALM x : c) y = eval' x (MULT y : c)
exec' (ADD x : c) y = exec' c (x + y)
exec' (MULT x : c) y = exec' c (x * y)

eval' :: Expr -> Cont -> Int
eval' (Val v) c = exec' c v
eval' (Add x y) c = eval' x (EVALA y : c)
eval' (Mult x y) c = eval' x (EVALM y : c)

value' :: Expr -> Int
value' e = eval' e []
