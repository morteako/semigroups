{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Mapping where

import           Semigroups2
import           Semigroups3

import           Data.Coerce
import           Data.Semigroup
import           Data.Monoid                    ( Ap(..) )
-- import           Data.Mod
import           Xor
-- import qualified Data.Bimap                    as Bi
import           Data.Invertible.TH
import           Data.Invertible.Bijection
import           Data.Proxy

data Two = T1 | T2 deriving (Eq,Enum)

instance Show Two where
  show T1 = "1"
  show T2 = "2"

data XYZ = X | Y | Z deriving (Show,Enum,Eq)

class Semigroup s => Mapping s t | s -> t where
    to :: s -> t
    to = biTo mapping

    from :: t -> s
    from = biFrom mapping

    res :: [[t]]

    mapping :: s <-> t
    mapping = undefined


checkTable3 :: forall s . Mapping s XYZ => [(s, s, String, s, s, s)]
checkTable3 = do
  let vs :: [s]
      vs = map from [X, Y, Z]
  x <- vs
  y <- vs
  z <- vs
  let a = (x <> (y <> z))
  let b = ((x <> y) <> z)
  if to a == to b then [] else [(a, b, "ab <-> xyz", x, y, z)]

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

boolToT1 False = T1
boolToT1 True  = T2

t1ToBool T1 = False
t1ToBool T2 = True

instance Mapping CH2 Two where
  to   = coerce boolToT1
  from = coerce t1ToBool

  res  = [[T1, T1], [T1, T2]]

instance Mapping C2 Two where
  to   = coerce boolToT1
  from = coerce t1ToBool

  res  = [[T1, T2], [T2, T1]]

instance Mapping O2 Two where
  to   = const T1
  from = const Proxy

  res  = [[T1, T1], [T1, T1]]


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
  res  = undefined

-- instance Mapping S3 XYZ where
--   to Null                   = X
--   to (NotNull Null        ) = Y
--   to (NotNull (NotNull ())) = Z

--   from X = Null
--   from Y = NotNull Null
--   from Z = NotNull (NotNull ())

--   res = [[Y, Z, Z], [Z, Z, Z], [Z, Z, Z]]

instance Mapping S4 XYZ where

  mapping = [biCase|
    (Ap Nothing           ) <-> Y
    (Ap (Just (Xor False))) <-> Z
    (Ap (Just (Xor True ))) <-> X
  |]

  res = [[Z, Y, X], [Y, Y, Y], [X, Y, Z]]

instance Mapping S5 XYZ where
  mapping = [biCase|
    (Nothing         ) <-> Y
    (Just (Xor False)) <-> Z
    (Just (Xor True )) <-> X
  |]

  res = [[Z, X, X], [X, Y, Z], [X, Z, Z]]

-- instance Mapping S6 XYZ where
--   mapping = [biCase|
--     Null <-> Y
--     (NotNull (Xor False)) <-> Z
--     (NotNull (Xor True )) <-> X
--   |]

--   res = [[Z, X, X], [X, Z, Z], [X, Z, Z]]

-- instance Mapping S7 XYZ where
--   mapping = [biCase|
--     Null                   <-> X
--     (NotNull Null        ) <-> Y
--     (NotNull (NotNull ())) <-> Z
--   |]

--   res = [[Z, Z, Z], [Z, Z, Z], [Z, Z, Z]]


-- instance Mapping S8 XYZ where
--   mapping = [biCase|
--        Null                  <-> Y
--      (NotNull (All False)) <-> X
--      (NotNull (All True )) <-> Z
--   |]

--   res = [[Z, Z, Z], [Z, Y, Z], [Z, Z, Z]]


-- instance Mapping S10 XYZ where
--   mapping = [biCase|
--      Nothing                  <-> Y
--      Just Null <-> X
--      Just (NotNull ()) <-> Z
--   |]

--   res = [[Z, X, Z], [X, Y, Z], [Z, Z, Z]]

-- instance Mapping S11 XYZ where
--   mapping = [biCase|
--      Ap Nothing                  <-> Y
--      Ap (Just Null) <-> X
--      Ap (Just (NotNull ())) <-> Z
--   |]

--   res = [[Z, Z, Z], [Y, Y, Y], [Z, Z, Z]]

instance Mapping S13 XYZ where
  mapping = [biCase|
    Max Nothing    <-> X
    Max (Just False) <-> Y
    Max (Just True)  <-> Z
  |]

  res = [[X, Y, Z], [Y, Y, Z], [Z, Z, Z]]

-- instance Mapping S15 XYZ where
--   mapping = [biCase|
--     Max Nothing    <-> Z
--     Max (Just False) <-> X
--     Max (Just True)  <-> Y
--   |]

--   res = [[X, X, X], [Y, Y, Y], [X, X, Z]]

instance Mapping S16 XYZ where
  mapping = [biCase|
    Ap Nothing    <-> Z
    Ap (Just (First False)) <-> X
    Ap (Just (First True))  <-> Y
  |]

  res = [[X, X, Z], [Y, Y, Z], [Z, Z, Z]]

instance Mapping S17 XYZ where
  mapping = [biCase|
    First Nothing    <-> X
    First (Just False) <-> Y
    First (Just True)  <-> Z
  |]

  res = [[X, X, X], [Y, Y, Y], [Z, Z, Z]]

instance Mapping S18 XYZ where
  mapping = [biCase|
    LT    <-> X
    EQ <-> Z
    GT  <-> Y
  |]

  res = [[X, X, X], [Y, Y, Y], [X, Y, Z]]


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

  putStrLn ""
  print "3elements"
  print "s1"
  print "s2"
  -- print $ testTable3 @S3
  print $ testTable3 @S4
  print $ testTable3 @S5
  -- print $ testTable3 @S6
  -- putStr "S7 : " >> print (testTable3 @S7)
  -- print "S7 missing"
  -- print $ testTable3 @S8
  print "S9 not"
  -- print $ testTable3 @S10
  -- print $ testTable3 @S11
  print "S12 not"
  print $ testTable3 @S13
  print "S14"
  print "S15"
  print $ testTable3 @S16
  print $ testTable3 @S17
  print $ testTable3 @S18


