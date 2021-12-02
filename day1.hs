countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [x] = 0
countIncreases (x:xs) =
  let count = if head xs > x then 1 else 0
  in count + countIncreases xs

sum3 :: [Int] -> [Int]
sum3 xs
  | size < 3  = []
  | otherwise = [sum (take 3 xs)] ++ sum3 (tail xs)
  where size = length xs

main = do
  input <- getContents
  let inputData = map read $ lines input
  let result = countIncreases inputData
  print result
  let result2 = countIncreases (sum3 inputData)
  print result2
