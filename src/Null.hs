{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Null where

import           Data.Semigroup
import           Data.Coerce



data Null m a = Null | NotNull a deriving Show

type Null' a = Null a a

instance (Monoid m, Semigroup a, Coercible m a) => Semigroup (Null m a) where
  (<>) :: Null m a -> Null m a -> Null m a
  Null      <> Null      = NotNull (coerce (mempty :: m))
  Null      <> a         = a
  a         <> Null      = a
  NotNull a <> NotNull b = NotNull (a <> b)


instance (Monoid m, Monoid a, Coercible m a) => Monoid (Null m a) where
  mempty = NotNull mempty

-- newtype Nulll m a = Nulll {getNulll :: Maybe a}



-- instance (Monoid m, Semigroup a, Coercible m a) => Semigroup (Nulll m a) where
--   (<>) :: Nulll m a -> Nulll m a -> Nulll m a
--   Nulll Nothing    <> Nulll Nothing    = NotNull (coerce (mempty :: m))
--   Nulll Nothing    <> a                = a
--   a                <> Nulll   Nothing  = a
--   NotNull (Just a) <> NotNull (Just b) = NotNull $ Just (a <> b)


-- instance (Monoid m, Monoid a, Coercible m a) => Monoid (Nulll m a) where
--   mempty = Nulll $ Just mempty
