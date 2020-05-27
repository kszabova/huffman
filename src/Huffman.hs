module Huffman where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Function

data Node = Leaf Char Int
          | Inner Node Node Int
          deriving (Eq, Show)

instance Ord Node where
    compare (Leaf c1 w1) (Leaf c2 w2)
        | c1 == c2  = compare w1 w2
        | otherwise = compare c1 c2
    compare (Leaf _ w1) (Inner _ _ w2)
        | w1 == w2  = LT
        | otherwise = compare w1 w2
    compare (Inner _ _ w1) (Leaf _ w2)
        | w1 == w2  = GT
        | otherwise = compare w1 w2
    compare (Inner _ _ w1) (Inner _ _ w2)
        = compare w1 w2

type CharWeights = [(Char, Int)]

data Bit = Zero | One deriving (Eq, Show)

weight :: Node -> Int
weight (Leaf _ w)    = w
weight (Inner _ _ w) = w

merge :: Node -> Node -> Node
merge n1 n2 = Inner n1 n2 (weight n1 + weight n2)

getFreqList :: String -> CharWeights
getFreqList s = Map.toList $ updateFreqs s $ Map.fromList []
    where updateFreqs "" l     = l
          updateFreqs (c:cs) l = updateFreqs cs $ Map.insertWith (+) c 1 l

sorted :: CharWeights -> CharWeights
sorted = List.sortBy (compare `on` snd)

makeTree :: CharWeights -> Node
makeTree = makeTree' . convert . sorted
    where makeTree' [n] = n
          makeTree' (n1:n2:ns)
            = makeTree' $ List.insert (merge n1 n2) ns

          convert = foldr (\(c, w) cs -> Leaf c w : cs) []

getEncodings :: Node -> Map.Map Char [Bit]
getEncodings t = Map.fromList $ getEncodings' t []
    where getEncodings' (Leaf c _) bits = [(c, bits)]
          getEncodings' (Inner n1 n2 _) bits
            = (getEncodings' n1 $ bits ++ [Zero]) ++ (getEncodings' n2 $ bits ++ [One])
