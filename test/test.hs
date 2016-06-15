import Network
import Data.Ratio
import Data.Char
import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad
import Debug.Trace
import System.IO

main = do
  pa <- prompt "Parent A Genotype: "
  pb <- prompt "Parent B Genotype: "
  putStrLn $ show $ cross (read pa) (read pb)
  
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

data GeneId = A | B | C | D | E | F deriving (Show, Eq, Read)
data Gene = Recessive {gid :: GeneId} | Dominant {gid :: GeneId} deriving (Eq)
data GenePair = GenePair {genes :: (Gene, Gene)} deriving (Eq)
data Genotype = Genotype {pairs :: [GenePair]} deriving (Eq)
data PossibleGenotypes = PossibleGenotypes {possible :: [(Genotype, Ratio Int)]} deriving (Eq)

instance Ord Gene where
  (Recessive _) `compare` (Dominant _) = LT
  (Dominant _) `compare` (Recessive _) = GT
  (Dominant _) `compare` (Dominant _) = EQ
  (Recessive _) `compare` (Recessive _) = EQ
  
instance Show Gene where
  show (Dominant a) = map toUpper . show $ a
  show (Recessive a) = map toLower . show $ a
  
instance Show GenePair where
  show (GenePair (a,b)) = show a ++ show b

instance Show Genotype where
  show g = show . pairs $ g
  
instance Show PossibleGenotypes where -- "[AA,BB] = 1/16"
  show g = gpart ++ ppart
    where poss = possible g 
          gpart = unlines $ zipWith (fancy) (map fst poss) (map snd poss)
          ppart = 
          phenos = zip (map (phenotype . fst) poss) (map snd poss)
          fancy a b = (show a) ++ " = " ++ (show $ numerator b) ++ "/" ++ (show $ denominator b)
  
instance Read Gene where
  readsPrec a b = [(parseGene $b!!0,"")]
  
instance Read GenePair where
  readsPrec a b = [((GenePair ((read [head b]),(read [head . reverse $ b]))),"")]
  
instance Read Genotype where
  readsPrec a b = [(   Genotype $ map (read) $ chunksOf 2 b   ,"")]
  
parseGene :: Char -> Gene
parseGene s | isUpper s = Dominant $ read [s]
            | isLower s = Recessive $ read [toUpper s]
  
-- pairs ga = [AA,Bb]
cross :: Genotype -> Genotype -> PossibleGenotypes -- [([AA,Bb], 1%2),([Aa,BB],1%2)]
cross ga gb = PossibleGenotypes $ nub [(Genotype g, rat) | g <- combos, rat <- [(length . filter (g==) $ combos) % length combos]]
  where possible = zipWith (possibleGenePairs) (pairs ga) (pairs gb)
        combos = sequence possible

possibleGenePairs :: GenePair -> GenePair -> [GenePair] -- [AB,Ab,aB,ab]
possibleGenePairs (GenePair (xa,xb)) (GenePair (ya,yb)) = [ fixPair $ GenePair (a,b) | a <- [xa,xb], b <- [ya,yb] ] 


fixPair :: GenePair -> GenePair
fixPair (GenePair (Recessive a, Dominant b)) = GenePair (Dominant b, Recessive a)
fixPair a = id a

phenotype :: GenePair -> String
phenotype (GenePair (Recessive a, Recessive _)) = "Recessive " ++ show a
phenotype (GenePair (a,_)) = "Dominant " ++ (show . gid $ a)
