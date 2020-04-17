{-# LANGUAGE DataKinds #-}

module Semigroups3 where

import           Data.Monoid                    ( Alt )
import           Xor
import           Semigroups2
import           Null
import           Zero
import           Data.Mod
import           Data.Semigroup





type S1 = C3
type C3 = Sum (Mod 3)


--S2 MISSING

type S3 = Null (Null' ()) O2

-- type S4 = Maybe CH2 --All
type S4 = Zero Xor --All

type S5 = Maybe C2

--Null
type S6 = Zero C2

type S7 = O3
-- type O3 = Proxy
type O3 = Null () O2


type S8 = Null' All


type S9 = Zero O2


type S10 = Maybe O2

--check mapping
-- type S11 = Null All (First Bool)
type S11 = Zero (Null () ())

type S12 = Maybe O2

type S13 = Max (Maybe Bool)

type S13' = Zero Any
type S13'' = Maybe Any
-- type S14 = 

type S15 = Maybe (First Bool)

type S16 = Zero (First Bool)

type S17 = First (Maybe Bool)

type S18 = Ordering
type S18Alt = Alt Maybe Bool
type S18Maybe = Maybe (First Bool)




