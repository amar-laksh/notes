module TautologyChecker (
    Prop (..),
    isTaut',
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

type Table = Assoc Char Bool

find' :: (Eq k) => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k == k']

eval' :: Table -> Prop -> Bool
eval' _ (Const b) = b
eval' t (Var var) = find' var t
eval' t (Not p) = not (eval' t p)
eval' t (And p q) = eval' t p && eval' t q
eval' t (Or p q) = eval' t p || eval' t q
eval' t (Imply p q) = eval' t p <= eval' t q
eval' t (Eqval p q) = eval' t (Imply p q) && eval' t (Imply q p)

bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False :) bss ++ map (True :) bss
  where
    bss = bools' (n - 1)

vars' :: Prop -> [Char]
vars' (Const _) = []
vars' (Var v) = [v]
vars' (Not p) = vars' p
vars' (And p q) = vars' p ++ vars' q
vars' (Or p q) = vars' p ++ vars' q
vars' (Eqval p q) = vars' p ++ vars' q
vars' (Imply p q) = vars' p ++ vars' q

rmdups' :: Eq el => [el] -> [el]
rmdups' [] = []
rmdups' (x : xs) = x : filter (/= x) (rmdups' xs)

table' :: Prop -> [Table]
table' p = map (zip vs) (bools' (length vs))
  where
    vs = rmdups' (vars' p)

isTaut' :: Prop -> Bool
isTaut' p = and [eval' s p | s <- table' p]
