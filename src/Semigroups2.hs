{-# LANGUAGE DataKinds #-}

module Semigroups2 where

import           Data.Semigroup
import           Data.Mod
import           Xor
import           Data.Proxy


-- type O2 = Null' ()
type O2 = Proxy ()



type LeftZero2 = First Bool

type RightZero2 = Last Bool

type Semilattice2 = CH2

type CH2 = All --eller Or
-- type CH2 = Zero ()






type C2 = Xor
type Z2 = Sum (Mod 2)
