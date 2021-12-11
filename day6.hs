import Common

calculateNextGeneration :: [Int] -> [Int]
calculateNextGeneration xs =
  let old = map (\x -> if x == 0 then 6 else (x-1)) xs
      new = map (const 8) (filter (\y -> y == 0) xs)
  in old ++ new

main = do
  input <- getContents
  let initialPopulation = map read $ splitOn ',' (head (lines input)) :: [Int]
  let finalPopulation = foldl (\acc _ -> calculateNextGeneration acc) initialPopulation [1..80]
  putStrLn $ "After 80 days: " ++ (show (length finalPopulation))
