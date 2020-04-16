module Xor where

newtype Xor = Xor {getXor :: Bool}

instance Semigroup Xor where
  Xor True  <> Xor True  = Xor False
  Xor False <> Xor False = Xor False
  Xor _     <> Xor _     = Xor True

instance Monoid Xor where
  mempty = Xor False
