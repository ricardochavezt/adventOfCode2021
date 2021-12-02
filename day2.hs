import Data.List

parseInstruction :: String -> (String, Int)
parseInstruction xs = case (words xs) of [] -> ("", 0)
                                         [x] -> (x, 0)
                                         (x:y:_) -> (x, read y)

followInstruction :: (Int, Int) -> (String, Int) -> (Int, Int)
followInstruction (x, y) (direction, distance) = case direction of "forward" -> (x+distance, y)
                                                                   "up" -> (x, y-distance)
                                                                   "down" -> (x, y+distance)

followInstruction' :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
followInstruction' (x, y, aim) (dir, dist) = case dir of "forward" -> (x+dist,y+aim*dist,aim)
                                                         "up" -> (x,y,aim-dist)
                                                         "down" -> (x,y,aim+dist)

main = do
  input <- getContents
  let instructions = map parseInstruction $ lines input
  let finalPos = foldl (\acc x -> followInstruction acc x) (0,0) instructions
  print finalPos
  let finalPos' = foldl (\acc x -> followInstruction' acc x) (0,0,0) instructions
  print finalPos'
