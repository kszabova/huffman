module Huffman where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Function
import Data.Char

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

getBits :: Node -> String -> [Bit]
getBits t = concat . map (\c -> encodings Map.! c)
    where encodings = getEncodings t

getString :: Node -> [Bit] -> String
getString tree bits = getString' tree tree bits ""
    where getString' _ (Leaf c _) [] s = s ++ [c]
          getString' _ _ [] s = s
          getString' r (Leaf c _) b s = getString' r r b (s ++ [c])
          getString' r (Inner left right _) (b:bs) s
            | b == Zero = getString' r left bs s
            | otherwise = getString' r right bs s

bitsToChar :: [Bit] -> Char
bitsToChar byte = chr $ bitsToChar' byte 0
    where bitsToChar' [] acc = acc
          bitsToChar' (b:bs) acc
            | b == Zero = bitsToChar' bs (2*acc)
            | otherwise = bitsToChar' bs (2*acc + 1)

charToBits :: Char -> [Bit]
charToBits char = charToBits' (ord char) []
    where charToBits' 0 bits = replicate (16 - length bits) Zero ++ bits
          charToBits' c bits
            | c `mod` 2 == 0 = charToBits' (c `div` 2) (Zero:bits)
            | otherwise      = charToBits' (c `div` 2) (One:bits)

extendToMultipleOf16 :: [Bit] -> [Bit]
extendToMultipleOf16 bits
    | (length bits) `mod` 16 == 0 = bits
    | otherwise = bits ++ replicate (16 - length bits `mod` 16) Zero

bitstreamToString :: [Bit] -> String
bitstreamToString bits = bsts (extendToMultipleOf16 bits) ""
    where bsts [] s = s
          bsts b s = bsts (drop 16 b) (s ++ [bitsToChar $ take 16 b])
