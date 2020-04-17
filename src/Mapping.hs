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

import           Data.Coerce
import           Data.Semigroup
import           Data.Monoid
import           Data.Mod
import           Xor
import           Null
import qualified Data.Bimap                    as Bi

data Two = T1 | T2 deriving (Eq,Enum)

instance Show Two where
  show T1 = "1"
  show T2 = "2"

data XYZ = X | Y | Z deriving (Show,Enum,Eq)

class Semigroup s => Mapping s t | s -> t where
    to :: s -> t

    from :: t -> s

    res :: [[t]]
    res = undefined



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

testTable :: forall s . Mapping s Two => Either ([[Two]], String, [[Two]]) ()
testTable = if r == table
  then Right ()
  else Left (r, " <exp res , actual > ", table)
 where
  r :: [[Two]]
  r     = res @s
  table = makeTable2 @s


testTable3 :: forall s . Mapping s XYZ => Either ([[XYZ]], String, [[XYZ]]) ()
testTable3 = if r == table
  then Right ()
  else Left (r, " <exp res , actual > ", table)
 where
  r :: [[XYZ]]
  r     = res @s
  table = makeTable3 @s

instance Mapping CH2 Two where
  to   = coerce boolToT1
  from = coerce t1ToBool

  res  = [[T1, T1], [T1, T2]]

instance Mapping C2 Two where
  to   = coerce boolToT1
  from = coerce t1ToBool

  res  = [[T1, T2], [T2, T1]]

instance Mapping O2 Two where
  to Null         = T2
  to (NotNull ()) = T1

  from T1 = NotNull ()
  from T2 = Null

  res = [[T1, T1], [T1, T1]]


boolToT1 False = T1
boolToT1 True  = T2

t1ToBool T1 = False
t1ToBool T2 = True

instance Mapping LeftZero2 Two where
  to   = coerce boolToT1
  from = coerce t1ToBool

  res  = [[T1, T1], [T2, T2]]

instance Mapping RightZero2 Two where
  to   = coerce boolToT1
  from = coerce t1ToBool

  res  = [[T1, T2], [T1, T2]]


instance Mapping S1 XYZ where
  to   = toEnum . fromEnum . getSum
  from = Sum . toEnum . fromEnum

instance Mapping S3 XYZ where
  to Null                   = X
  to (NotNull Null        ) = Y
  to (NotNull (NotNull ())) = Z

  from X = Null
  from Y = NotNull Null
  from Z = NotNull (NotNull ())

  res = [[Y, Z, Z], [Z, Z, Z], [Z, Z, Z]]

instance Mapping S4 XYZ where
  to (Ap Nothing           ) = Y
  to (Ap (Just (Xor False))) = Z
  to (Ap (Just (Xor True ))) = X

  from Y = (Ap Nothing)
  from Z = (Ap $ Just (Xor False))
  from X = (Ap $ Just (Xor True))

  res = [[Z, Y, X], [Y, Y, Y], [X, Y, Z]]

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
--   f $ makeTable3 @S1
--   f $ makeTable3 @S13
--   print $ testTable @CH2
--   print $ testTable @C2

  print "2 elements"
  print $ testTable @O2
  print $ testTable @C2
  print $ testTable @CH2
  print $ testTable @LeftZero2
  print $ testTable @RightZero2

  print "3elements"
  print $ testTable3 @S3
  print $ testTable3 @S4
