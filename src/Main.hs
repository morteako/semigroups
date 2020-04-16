module Main where

import           Semigroups2
import           Semigroups3
import           Mapping
import           Null
import           Data.Semigroup

check xs = do
  x <- xs
  return $ do
    x' <- xs
    return (x <> x')

main :: IO ()
main = do
  putStrLn "hello world"
  -- mapM_ print $ check [Null :: Null () (), NotNull ()]
  -- mapM_ print $ check3 toTest fromTest
  -- mapM_ print $ check [Nothing, Just (First False), Just (First True)]

  mapM_ (print . fmap getMax) $ check $ fmap Max
                                             [Nothing, Just False, Just True]
