{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Mapping where

import           Semigroups2
import           Semigroups3

import           Data.Semigroup
import           Data.Mod
import           Xor
import           Null

data Two = T1 | T2 deriving Enum

instance Show Two where
  show T1 = "1"
  show T2 = "2"

data XYZ = X | Y | Z deriving (Show,Enum)

class Semigroup s => Mapping s t | s -> t where
    to :: s -> t
    from :: t -> s

makeTable3 :: forall s . Mapping s XYZ => [[XYZ]]
makeTable3 = do
  let vs :: [s]
      vs = map from [X, Y, Z]
  x <- vs
  return $ to <$> do
    y <- vs
    return (x <> y)

makeTable2 :: forall s . Mapping s Two => [[Two]]
makeTable2 = do
  let vs :: [s]
      vs = map from [T1, T2]
  x <- vs
  return $ to <$> do
    y <- vs
    return (x <> y)

instance Mapping CH2 Two where
  to (All False) = T1
  to (All True ) = T2

  from T1 = All False
  from T2 = All True

instance Mapping C2 Two where
  to (Xor False) = T1
  to (Xor True ) = T2

  from T1 = Xor False
  from T2 = Xor True

instance Mapping O2 Two where
  to Null         = T1
  to (NotNull ()) = T2

  from T1 = Null
  from T2 = NotNull ()


instance Mapping S1 XYZ where
  to   = toEnum . fromEnum . getSum
  from = Sum . toEnum . fromEnum

instance Mapping S13 XYZ where
  to (Max m) = case m of
    Nothing    -> X
    Just False -> Y
    Just True  -> Z
  from xyz = Max $ case xyz of
    X -> Nothing
    Y -> Just False
    Z -> Just True

-- instance Mapping S5 where
--     to (Maybe )

f table = do
  mapM_ (putStrLn . foldMap show) table
  putStrLn ""

test = do
  f $ makeTable3 @S1
  f $ makeTable3 @S13
  f $ makeTable2 @CH2
  f $ makeTable2 @C2

  f $ makeTable2 @O2
