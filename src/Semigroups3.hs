{-# LANGUAGE DataKinds #-}

module Semigroups3 where

import           Data.Monoid                    ( Alt )
import           Xor
import           Semigroups2
import           Zero
import           Data.Mod
import           Data.Semigroup





type S1 = C3
type C3 = Sum (Mod 3)


--S2 MISSING

--S3 Missing

-- type S4 = Maybe CH2 --All
type S4 = Zero Xor --All

type S5 = Maybe C2


--S6 missing
-- type S6 = Null Xor C2

--S7 missing


--S8 missing


type S9 = Zero O2


type S10 = Maybe O2


--Missing
-- type S11 = 

--Missing
-- type S12 = Maybe O2

type S13 = Max (Maybe Bool)

type S13' = Zero Any
type S13'' = Maybe Any

--Missing
-- type S14 = 

--Wrong
type S15 = Maybe (First Bool)

type S16 = Zero (First Bool)

type S17 = First (Maybe Bool)

type S18 = Ordering
type S18Alt = Alt Maybe Bool
type S18Maybe = Maybe (First Bool)




