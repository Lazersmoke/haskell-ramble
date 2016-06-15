-- AL chance is 3%5
-- NL chance is 2%5
import Data.Ratio
import Data.List.Split
import Data.List
import Control.Monad
import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), )

data Winner = American | National deriving (Show, Eq, Ord)
type Game = [Winner]

cullGame :: Game -> Game
cullGame x = chop [American] . chop [National] $ x
  where chop y a = intercalate y . take 4 $ splitOn y a

-- chop truncates to 4 wins
  
  
--singleGame :: Dist
singleGame = Dist.fromFreqs [(American, 3%5),(National, 2%5)]

multipleGames = flip replicateM singleGame