import Common

parsePoint :: String -> (Int,Int)
parsePoint s = case (splitOn ',' s) of (x:y:_) -> (read x, read y)
                                       xs -> (0,0)

parseSegment :: String -> ((Int,Int),(Int,Int))
parseSegment line = case (words line) of (x:"->":y:_) -> (parsePoint x, parsePoint y)
                                         xs -> ((0,0),(0,0))

commonPoints :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int)) -> Int
commonPoints ((xa,ya),(xb,yb)) ((xc,yc),(xd,yd)) =
  let commonX = max ((min y1 y1') - (max y0' y0) + 1) 0
      commonY = max ((min x1 x1') - (max x0' x0) + 1) 0
  in commonX * commonY
  where x0 = min xa xb
        y0 = min ya yb
        x1 = max xa xb
        y1 = max ya yb
        x0' = min xc xd
        y0' = min yc yd
        x1' = max xc xd
        y1' = max yc yd

pointOnSegment :: (Int,Int) -> ((Int,Int),(Int,Int)) -> Bool
pointOnSegment (x,y) ((x0,y0),(x1,y1))
  | x0 == x1  = x0 == x && between y y0 y1
  | y0 == y1  = y0 == y && between x x0 x1
  | otherwise = crossproduct == 0 && dotproduct >= 0 && dotproduct <= squaredlength
  where between x' a b = (min a b) <= x' && x' <= (max a b)
        crossproduct = ((y - y0) * (x1 - x0)) - ((x - x0) * (y1 - y0))
        dotproduct = ((x - x0) * (x1 - x0)) + ((y - y0) * (y1 - y0))
        squaredlength = (x1 - x0)*(x1 - x0) + (y1 - y0)*(y1 - y0)

nLinesCoveringPoint :: (Int,Int) -> [((Int,Int),(Int,Int))] -> Int
nLinesCoveringPoint point lines =
  let linesCoveringPoint = filter (\line -> pointOnSegment point line) lines
  in length linesCoveringPoint

main = do
  input <- getContents
  let segments = map parseSegment $ lines input
  let hOrVSegments = filter (\((x0,y0),(x1,y1)) -> x0 == x1 || y0 == y1) segments

  let (maxX,maxY) = foldl (\(accX, accY) ((x0,y0),(x1,y1)) -> ((max accX x0),(max accY y0))) (0,0) segments

  let grid = concat $ map (\x -> map (\y -> (x,y)) [0..maxY]) [0..maxX]
  -- let crossPoints = filter (\p -> (nLinesCoveringPoint p hOrVSegments) > 1) grid
  -- putStrLn $ (show (length crossPoints)) ++ " puntos con cruces (solo horizontales & verticales)"
  let crossPoints' = filter (\p -> (nLinesCoveringPoint p segments) > 1) grid
  putStrLn $ (show (length crossPoints')) ++ " puntos con cruces (todos)"
