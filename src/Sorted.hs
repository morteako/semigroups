module Sorted where

data Sorted = None | Both | Down | Up

instance Semigroup Sorted where
  x <> y = case x of
    Both -> y
    None -> y
    Down -> case y of
      Up   -> None
      None -> None
      _    -> Down
    Up -> case y of
      Down -> None
      None -> None
      _    -> Up


instance Monoid Sorted where
  mempty = Both



