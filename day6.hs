import Common
import Data.List
import qualified Data.Map as Map

calculateNextGenerationCounts :: Map.Map Int Int -> Map.Map Int Int
calculateNextGenerationCounts m = Map.fromList $ map (\x -> (x, calc x)) [0..8]
  where calc 8 = Map.findWithDefault 0 0 m
        calc 6 = (Map.findWithDefault 0 7 m) + (Map.findWithDefault 0 0 m)
        calc x = Map.findWithDefault 0 (x+1) m

main = do
  input <- getContents
  let initialPopulation = map read $ splitOn ',' (head (lines input)) :: [Int]
  let initialPopulationCounts = Map.fromList $ map (\l@(x:xs) -> (x, length l)) . group . sort $ initialPopulation
  let finalPopulationCounts = foldl (\acc _ -> calculateNextGenerationCounts acc) initialPopulationCounts [1..80]
  putStrLn $ "After 80 days: " ++ (show (sum (Map.elems finalPopulationCounts)))
  let finalPopulationCounts' = foldl (\acc _ -> calculateNextGenerationCounts acc) finalPopulationCounts [81..256]
  putStrLn $ "After 256 days: " ++ (show (sum (Map.elems finalPopulationCounts')))
