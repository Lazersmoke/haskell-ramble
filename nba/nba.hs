import Text.CSV
import Text.Read
import Data.Char
import Data.Ratio
import Data.List
import Data.Either

type Table = [Col]
type Col = (Label,ColData)
type ColData = [String]
type Label = String

grabCol :: Table -> Label -> Col
grabCol csv label = head . filter ((==label) . fst) $ csv

grabCols :: Table -> [Label] -> Table
grabCols table lbs = filter ((`elem` lbs) . fst) table

pairify :: Table -> [(String,String)]
pairify = map (\[x,y] -> (x,y)) . transpose . map snd

main = do
  Right parsed <- parseCSVFromFile "nba.csv"
  print . manip $ parsed

parseIt :: IO CSV
parseIt = do
  Right parsed <- parseCSVFromFile "nba.csv"
  return parsed

manip :: CSV -> Table
manip csv = zip (head csv) . transpose . filter (all isDigit . head) $ csv
{-
recordToPlayer :: Record -> Player
recordToPlayer rec = Player (r 1) (r 2) (r 4) (r 6) (r 7 % r 8) (r 10 % r 11) (r 13 % r 14) (read $ r 16) (read $ r 17) (read $ r 18) (read $ r 19) (read $ r 20) (read $ r 21) (read $ r 22) (read $ r 23) (read $ r 24)
  where
    r = (rec !!)
    n = read . r
-}
