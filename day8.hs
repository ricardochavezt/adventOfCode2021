import Common
import Data.List
import qualified Data.Map as Map

decodePattern :: String -> String -> String -> Int
decodePattern s p1 p4
  | size == 2 = 1
  | size == 3 = 7
  | size == 4 = 4
  | size == 5 && all (\x -> x `elem` s) p1 = 3
  | size == 5 && all (\x -> x `elem` s) p4 = 5
  | size == 5 = 2
  | size == 6 && all (\x -> x `elem` s) (p1 ++ p4) = 9
  | size == 6 && all (\x -> x `elem` s) p1 = 0
  | size == 6 = 6
  | otherwise = 8
  where size = length s

decodePatterns ps = Map.fromList $ map (\p -> ((sort p), (decodePattern p p1 p4))) ps
  where p1 = head (filter (\p' -> (length p') == 2) ps)
        p4 = head (filter (\p' -> (length p') == 4) ps) \\ p1

decodeNumber ps patternMap = foldl1 (\a c -> 10*a+c) $ map (\p -> patternMap Map.! (sort p)) ps

main = do
  input <- getContents
  let entries = map (\l -> let (s:v) = splitOn '|' l in [(words s), (words (head v))]) $ lines input
  let outputValues = map (\(s:v:_) -> v) entries
  putStr "Part 1: "
  print $ sum $ map (\x -> length (filter (\y -> (length y) `elem` [2,3,4,7]) x)) outputValues
  putStr "Part 2: "
  print $ sum $ map (\(s:v:_) -> let pm = decodePatterns s in decodeNumber v pm) entries
