import Text.CSV
import Data.List
import Control.Applicative
import Data.Ratio

main = do
  Right parsed <- parseCSVFromFile "Lottery.csv"
  print . counted $ parsed

counted :: CSV -> [(Ratio Int, Int)]
counted c = countIt . readList . dropDateMult . removeNull . breakup . takeMid $ c
  where
    breakup = map (concatMap words)
    removeNull = filter (not . null)
    dropDateMult = map takeMid
    readList = map read . concat 
    countIt = map ((,) <$> (%count) . length <*> head) . group . sort
    -- Use extra parens to force section
    count = length c
takeMid = init . tail
