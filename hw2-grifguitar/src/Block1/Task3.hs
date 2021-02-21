{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
  ( NonEmpty(..)
  ) where

data NonEmpty a = a :| [a]

instance (Eq a) => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (==) (x :| xs) (y :| ys) = (x == y) && (xs == ys)

instance (Show a) => Show (NonEmpty a) where
  show :: NonEmpty a -> String
  show (x :| xs) = show x ++ show xs

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = f x :| (<*>) fs xs

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc (x :| xs) = f x (foldr f acc xs)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse wrap (x :| xs) = (<*>) (fmap (:|) (wrap x)) (traverse wrap xs)

add :: NonEmpty a -> [a] -> NonEmpty a
add (x :| xs) ys = x :| (xs ++ ys)

convert :: NonEmpty a -> [a]
convert (x :| xs) = x : xs

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| xs) action = add (action x) ((>>=) xs (convert . action))