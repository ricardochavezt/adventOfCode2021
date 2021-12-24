import Data.Char
import Data.List

isLowPoint :: Int -> [Int] -> Bool
isLowPoint _ [] = False
isLowPoint _ [x] = True
isLowPoint 0 (x0:x1:xs) = x0 < x1
isLowPoint pos xs
  | v == 0 = True
  | pos == ((length xs) - 1) = v < vPrev
  | otherwise = v < vPrev && v < (xs !! (pos+1))
  where v = xs !! pos
        vPrev = xs !! (pos-1)

filterLowPoints :: [Int] -> [(Int,Int)]
filterLowPoints ps = filter (\(pos,_) -> isLowPoint pos ps) $ zip [0..] ps

main = do
  input <- getContents
  let heightMap = map (\l -> map digitToInt l) $ lines input
  let lowPointsH = concatMap (\(x,row) -> map (\(y,val) -> (x,y,val)) (filterLowPoints row)) (zip [0..] heightMap)
  let heightMap' = transpose heightMap
  let lowPoints = filter (\(x,y,val) -> isLowPoint x (heightMap' !! y)) lowPointsH
  let riskLevel = (foldl (\acc (_,_,v) -> acc + v) 0 lowPoints) + (length lowPoints)
  putStrLn $ "Risk level: " ++ (show riskLevel)
