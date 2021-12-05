import Data.List
import Data.Char

parseBits :: [Int] -> Int
parseBits bits = foldl (\acc b -> acc*2+b) 0 bits

parseBitString :: String -> Int
parseBitString bitString = parseBits $ map digitToInt bitString

parseLine :: String -> (Int, Int)
parseLine line = foldl (\(z, o) x -> if x == '0' then (z+1, o) else (z, o+1)) (0,0) line

sumLines :: [String] -> [(Int, Int)]
sumLines inputLines = map parseLine $ transpose inputLines

filterInputs :: ((Int,Int) -> Char) -> [String] -> Int -> [String]
filterInputs f inputs i =
  let reportSum = sumLines inputs
  in filter (\input -> (input !! i) == (f $ reportSum !! i)) inputs

filterInputsRecursive :: Int -> [String] -> ([String] -> Int -> [String]) -> [String]
filterInputsRecursive _ [] _ = []
filterInputsRecursive _ [x] _ = [x]
filterInputsRecursive i inputs f =
  let nextInputs = f inputs i
  in filterInputsRecursive (i+1) nextInputs f

getRating :: [String] -> ([String] -> Int -> [String]) -> [String]
getRating = filterInputsRecursive 0

main = do
  input <- getContents
  let inputLines = lines input
  let reportSum = sumLines inputLines
  let gammaRate = parseBits $ map (\(z,o) -> if z > o then 0 else 1) reportSum
  let epsilonRate = parseBits $ map (\(z,o) -> if z < o then 0 else 1) reportSum
  putStrLn $ "gamma rate: " ++ (show gammaRate) ++ ", epsilon rate: " ++ (show epsilonRate)
  putStrLn $ "power consumption: " ++ (show (gammaRate * epsilonRate))

  let o2GenRating = parseBitString $ head $
                    getRating inputLines (filterInputs (\(z,o) -> if z > o then '0' else '1'))
  let co2ScrubRating = parseBitString $ head $
                       getRating inputLines (filterInputs (\(z,o) -> if z <= o then '0' else '1'))

  putStrLn $ "oxygen generator rating: " ++ (show o2GenRating) ++ ", CO2 scrubber rating: " ++ (show co2ScrubRating)
  putStrLn  $ "life support rating: " ++ (show (o2GenRating * co2ScrubRating))
