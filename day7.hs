import Common
import Data.List

median :: [Int] -> [Int]
median xs
  | odd n = [xs !! (n `div` 2)]
  | otherwise = [xs !! ((n `div` 2) - 1), xs !! (n `div` 2)]
  where n = length xs

cost1 x1 x2 = abs (x1-x2)
cost2 x1 x2 = n * (n+1) `div` 2
  where n = abs (x1-x2)

fuelCost f p ps = map (f p) ps

totalFuelCost f p ps = sum . (map (f p)) $ ps

main = do
  input <- getContents
  let positions = map read $ splitOn ',' (head (lines input)) :: [Int]
  let md = median (sort positions)

  print $ minimum (map (\x -> totalFuelCost cost1 x positions) md)

  let minPos = minimum positions
      maxPos = maximum positions
  print $ minimum (map (\x -> totalFuelCost cost2 x positions) [minPos..maxPos])
