module Common
  ( splitOn
  ) where

import Data.List
import Data.Function

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x ys = filter (not . any (== x)) . groupBy ((==) `on` (== x)) $ ys
