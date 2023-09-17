module Chapter12 (
    Chapter12.fmap,
    Chapter12.MyMaybe (..),
)
where

class MyFunctor f where
    fmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
    fmap = map

data MyMaybe a = Some a | None

instance Functor MyMaybe where
    fmap _ None = None
    fmap f (Some x) = Some (f x)

instance Applicative MyMaybe where
    pure = Some
    None <*> _ = None
    (Some f) <*> g = Prelude.fmap f g
