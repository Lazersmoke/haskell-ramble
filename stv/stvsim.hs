
import Data.Ratio
type Ballot = [Candidate]
-- Econ,Social
type Score = Integer
type Spectrum = (Score, Score)
type VotingResult = [(Candidate, Ratio Int)]
data Candidate = Candidate {name :: String, spectrum :: Spectrum} deriving (Eq,Read)

instance Show Candidate where
  show = name

candidates2016 :: [Candidate]
candidates2016 = [
  Candidate {name = "Donald Trump", spectrum = (70,30)},
  Candidate {name = "Jill Stein", spectrum = (0,100)},
  Candidate {name = "Bernie Sanders", spectrum = (10,100)},
  Candidate {name = "Marco Rubio", spectrum = (80,10)},
  Candidate {name = "John Kasich", spectrum = (80,40)},
  Candidate {name = "Carly Fiorina", spectrum = (80,30)},
  Candidate {name = "Ted Cruz", spectrum = (100,20)},
  Candidate {name = "Hillary Clinton", spectrum = (10,80)},
  Candidate {name = "Ben Carson", spectrum = (80,20)},
  Candidate {name = "Joe Biden", spectrum = (10,80)}
  ]


tallyVotes :: [Candidate] ->  VotingResult
tallyVotes votes = map (\x -> (x, (% length votes) . length . filter (==x) $ votes)) candidates2016

doSTV :: [Ballot] -> VotingResult -> Candidate
doSTV ballots results = doSTV (map safeTail ballots) . removeLeast . tallyVote . mapMaybe safeHead $ ballots

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:x) = Just x
