countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [x] = 0
countIncreases (x:xs) =
  let count = if head xs > x then 1 else 0
  in count + countIncreases xs

main = do
  input <- getContents
  let inputData = map read $ lines input
  let result = countIncreases inputData
  print result
