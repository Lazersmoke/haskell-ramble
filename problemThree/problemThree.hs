import qualified Numeric.Probability.Distribution as Dist
import Control.Monad
import Data.Ratio
data Child = Male | Female deriving (Ord, Eq, Show)
type Family = [Child]
main = interact $ \x -> unlines $ map (show . Dist.decons . testCase . read) (lines x)
singleChild :: Dist.T (Ratio Int) Child
singleChild = Dist.uniform [Male,Female]

family :: Int -> Dist.T (Ratio Int) Family
family = (flip replicateM) singleChild

isValidFamily :: Family -> Bool
isValidFamily = not . elem Female

testCase :: Int -> Dist.T (Ratio Int) Bool
testCase s = Dist.map isValidFamily $ family s

-- TODO: make fancy
makeOutput :: Dist.T (Ratio Int) Bool -> String
makeOutput x = show . Dist.decons$x 
