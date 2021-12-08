import Data.List
import Data.Function

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x ys = filter (not . any (== x)) . groupBy ((==) `on` (== x)) $ ys

checkNumbersH :: (Eq a) => [a] -> [[a]] -> Bool
checkNumbersH l b = case result of Just _ -> True
                                   Nothing -> False
  where result = find (\x -> all (`elem` l) x) b

checkNumbers :: (Eq a) => [a] -> [[a]] -> Bool
checkNumbers l b = (checkNumbersH l b) || (checkNumbersH l (transpose b))

checkAllBoardsStep :: (Eq a) => [[[a]]] -> (Maybe [[a]], [a]) -> [a] -> (Maybe [[a]], [a])
checkAllBoardsStep _ all@(Just x, l) _ = all
checkAllBoardsStep bs (acc, _) l = ((find (checkNumbers l) bs), l)

sumUnmarked :: (Num a, Eq a) => Maybe [[a]] -> [a] -> a
sumUnmarked Nothing _ = 0
sumUnmarked (Just wb) wn = sum $ concatMap (filter (\x -> not (x `elem` wn))) wb

main = do
  input <- getContents
  let (numbers:boardLines) = lines input
  let numberList = map read $ splitOn ',' numbers :: [Int]
  let boards = map (\xs -> map (\y -> map read (words y)) xs) $ splitOn "" boardLines :: [[[Int]]]

  let (winnerBoard, winnerNums) = foldl (checkAllBoardsStep boards) (Nothing, []) $ inits numberList

  print $ (sumUnmarked winnerBoard winnerNums) * (last winnerNums)
