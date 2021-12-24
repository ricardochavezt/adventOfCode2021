import Data.List

matchClosing '{' '}' = True
matchClosing '(' ')' = True
matchClosing '[' ']' = True
matchClosing '<' '>' = True
matchClosing _ _ = False

parseStep :: Maybe String -> Char -> Maybe String
parseStep Nothing _ = Nothing
parseStep (Just line) c = if c `elem` "([{<" then Just (line ++ [c]) else checkClosing
  where checkClosing = if (matchClosingOpt line c) then Just (init line) else Nothing
        matchClosingOpt line c = if (null line) then False else matchClosing (last line) c

checkSyntax :: String -> (Maybe String, Char)
checkSyntax line = foldl (\(accLine, lastChar) curr -> ((parseStep accLine curr), if accLine == Nothing then lastChar else curr)) ((Just ""), '_') line

illegalCharacterScore :: Char -> Int
illegalCharacterScore ')' = 3
illegalCharacterScore ']' = 57
illegalCharacterScore '}' = 1197
illegalCharacterScore '>' = 25137
illegalCharacterScore _ = 0

getClosingChar :: Char -> Char
getClosingChar '{' = '}'
getClosingChar '(' = ')'
getClosingChar '[' = ']'
getClosingChar '<' = '>'
getClosingChar _ = ' '

getClosingString :: String -> String
getClosingString s = foldr (\c acc -> acc ++ [getClosingChar c]) "" s

closingCharacterScore :: Char -> Int
closingCharacterScore ')' = 1
closingCharacterScore ']' = 2
closingCharacterScore '}' = 3
closingCharacterScore '>' = 4
closingCharacterScore _ = 0

getClosingStringScore :: String -> Int
getClosingStringScore s = foldl (\acc c -> acc * 5 + (closingCharacterScore c)) 0 s

main = do
  input <- getContents
  let parsedLines = map checkSyntax $ lines input
  let (syntaxErrorLines,incompleteLines) = partition (\(ms, _) -> ms == Nothing) parsedLines
  let syntaxErrorScore = foldl (\acc (_, c)-> acc + (illegalCharacterScore c)) 0 syntaxErrorLines
  putStrLn $ "Part 1 score: " ++ (show syntaxErrorScore)
  let closingStringScores = sort $ map (\(Just s, _) -> ((getClosingStringScore . getClosingString) s) ) incompleteLines
  let middleScore = closingStringScores !! ((length closingStringScores) `div` 2)
  putStrLn $ "Part 2 score: " ++ (show middleScore)
