import Data.List
import Data.Char

parseLine :: String -> (Int, Int)
parseLine line = foldl (\(z, o) x -> if x == '0' then (z+1, o) else (z, o+1)) (0,0) line

sumLines :: [String] -> [(Int, Int)]
sumLines inputLines = map parseLine $ transpose inputLines

parseBits :: [Int] -> Int
parseBits bits = foldl (\acc b -> acc*2+b) 0 bits

parseBitString :: String -> Int
parseBitString bitString = parseBits $ map digitToInt bitString

filterInputs :: [String] -> Int -> Char -> [String]
filterInputs [] _ _ = []
filterInputs [x] _ _ = [x]
filterInputs inputs i c = filter (\input -> (input !! i) == c) inputs

filterInputs' :: [String] -> Int -> [String]
filterInputs' inputs i =
  let reportSum = sumLines inputs
  in filterInputs inputs i (let (z,o) = reportSum !! i in (if z > o then '0' else '1'))

filterInputsRecursive :: [String] -> Int -> [String]
filterInputsRecursive [] _ = []
filterInputsRecursive [x] _ = [x]
filterInputsRecursive inputs i =
  let nextInputs = filterInputs' inputs i
  in filterInputsRecursive nextInputs (i+1)

filterInputs'' :: [String] -> Int -> [String]
filterInputs'' inputs i =
  let reportSum = sumLines inputs
  in filterInputs inputs i (let (z,o) = reportSum !! i in (if z <= o then '0' else '1'))

filterInputsRecursive' :: [String] -> Int -> [String]
filterInputsRecursive' [] _ = []
filterInputsRecursive' [x] _ = [x]
filterInputsRecursive' inputs i =
  let nextInputs = filterInputs'' inputs i
  in filterInputsRecursive' nextInputs (i+1)

main = do
  input <- getContents
  let inputLines = lines input
  let reportSum = sumLines inputLines
  let gammaRate = parseBits $ map (\(z,o) -> if z > o then 0 else 1) reportSum
  let epsilonRate = parseBits $ map (\(z,o) -> if z < o then 0 else 1) reportSum
  putStrLn $ "gamma rate: " ++ (show gammaRate) ++ ", epsilon rate: " ++ (show epsilonRate)
  putStrLn $ "power consumption: " ++ (show (gammaRate * epsilonRate))

  let o2GenRating = parseBitString $ head $ filterInputsRecursive inputLines 0
  let co2ScrubRating = parseBitString $ head $ filterInputsRecursive' inputLines 0

  putStrLn $ "oxygen generator rating: " ++ (show o2GenRating) ++ ", CO2 scrubber rating: " ++ (show co2ScrubRating)
  putStrLn  $ "life support rating: " ++ (show (o2GenRating * co2ScrubRating))
