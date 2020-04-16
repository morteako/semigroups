module Zero where

import           Data.Monoid                    ( Ap )

type Zero a = Ap Maybe a
