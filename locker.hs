-- locks and unlocks a value with a key

module Locker (lock, unlock, Locker) where

data Locker a = Locker Int a

instance (Show a) => Show (Locker a) where
 show _ = "*SECRET*"

instance Functor Locker where
 fmap f (Locker key val) = Locker key (f val)

lock :: Int -> a -> Locker a
lock key val = Locker key val

unlock :: (Eq a) => Int -> Locker a -> Maybe a
unlock x (Locker key val) =
       if x /= key then Nothing
       else Just val
