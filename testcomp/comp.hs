import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid

import Debug.Trace

data Tree a = TreeValue a | Node (m a) [Tree a] 

instance Show a => Show (Tree a) where
  show (TreeValue a) = show a 
  show theNode@(Node op inputs) = "Node with result {" ++ show (getValue theNode) ++ "} and inputs: {" ++ show inputs ++ "}"

evalTree :: Tree a -> Tree a
evalTree (Node op inputs) = TreeValue $ op (map getValue inputs)
evalTree (TreeValue n) = TreeValue n

getValue :: Tree a -> a
getValue (Node op inputs) = getValue $ evalTree (Node op inputs)
getValue (TreeValue n) = n

addChild :: Tree a -> Tree a -> Tree a
addChild newChild (Node op inputs) = Node op (newChild : inputs)
addChild newChild (TreeValue n) = Node (foldl1 const) (newChild : [TreeValue n])

addParent :: Tree a -> Tree a -> Tree a
addParent newParent oldTree = addChild oldTree newParent

changeOp :: Tree a -> ([a] -> a) -> Tree a
changeOp (Node _ inputs) newOp = Node newOp inputs
changeOp (TreeValue n) newOp = Node newOp [TreeValue n]

testTree :: Tree Double
testTree = Node product [TreeValue 2, Node sum $ map TreeValue [1,2,3,4,5,6]]

parseTest :: Tree Double
parseTest = ta (tg '4') . tp (tg '*') . ta (tg '3') . tp (tg '+') $ tg '1'

specialSymbols :: Floating a => Map.Map Char ([a] -> a)
specialSymbols = Map.fromList [
  ('*', product),
  ('+', sum),
  ('^', foldl1 (**)),
  ('-', foldl1 (-)),
  ('/', foldl1 (/))]

getTreeFromChar :: Char -> Tree Double
getTreeFromChar c = if c `Map.member` specialSymbols
  then Node (fromJust $ Map.lookup c specialSymbols) []
  else TreeValue $ read [c]

getTreeFromString :: String -> Tree Double
getTreeFromString str = foldl (\x y -> if isNode y then tp y x else ta y x) (TreeValue 0) treeList 
  where
    treeList = map getTreeFromChar str 

isNode :: Tree a -> Bool
isNode (Node _ _) = True
isNode (TreeValue _) = False

tg = getTreeFromChar
ta = addChild
tp = addParent
  
main = return ()
