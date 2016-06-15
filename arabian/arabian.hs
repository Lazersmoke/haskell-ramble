import Codec.Binary.UTF8.String as U
import Data.Function
import System.IO.Unsafe
import Data.Char
import Data.List
import Control.Monad
readTheThing :: [String]
readTheThing = filter (flip notElem stopWords) . words . map toLower . filter goodChar $ unsafePerformIO $ readFile "Arabian Nights.txt"

goodChar :: Char -> Bool
goodChar c = isLetter c || isSpace c

stopWords :: [String]
stopWords = words . unsafePerformIO $ readFile "Stop.txt"

countWord :: String -> [String] -> Int
countWord search base = length $ filter (==search) base
main = putStrLn . show $ countAll readTheThing
countAll :: [String] -> [(String, Int)]
countAll base = sortBy (compare `on` snd) $ nub [(word,countWord word base) | word <- (nub base)] 
