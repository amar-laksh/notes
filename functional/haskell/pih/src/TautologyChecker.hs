module TautologyChecker
  ( Prop (..),
  )
where

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Eqval Prop Prop
  | Imply Prop Prop
  deriving (Show)

type Assoc k v = [(k, v)]

type Table k v = Assoc Char Bool

find' :: (Eq k) => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k == k']

eval' :: Table k v -> Prop -> Bool
eval' _ (Const b) = b
eval' t (Var var) = find' var t
eval' t (Not p) = not (eval' t p)
eval' t (And p q) = eval' t p && eval' t q
eval' t (Or p q) = eval' t p || eval' t q
eval' t (Imply p q) = eval' t p <= eval' t q
eval' t (Eqval p q) = eval' t (Imply p q) && eval' t (Imply q p)
