module Huffman (encode, decode) where

import qualified Data.Map        as Map
import qualified Data.List       as List
import qualified Data.ByteString as BS

import Data.Function
import Data.Word


-- Data definitions
type CharWeights = [(Char, Int)]
data Bit = Zero | One deriving (Eq, Show)
data Node = Leaf Char Int
          | Inner Node Node Int
          deriving (Eq, Show, Read)

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


-- Constant storing the size of one word
wordSize :: Int
wordSize = 8


-- Get weight of Node
weight :: Node -> Int
weight (Leaf _ w)    = w
weight (Inner _ _ w) = w


-- merge :: left -> right -> parent
-- Takes two nodes and creates a new one
-- with the arguments as left and right children, respectively
merge :: Node -> Node -> Node
merge n1 n2 = Inner n1 n2 (weight n1 + weight n2)


-- Parses string and returns its character frequencies
-- in a list of (char, freq) tuples
getFreqList :: String -> CharWeights
getFreqList s = Map.toList $ updateFreqs s $ Map.fromList []
    where updateFreqs "" l     = l
          updateFreqs (c:cs) l = updateFreqs cs $ Map.insertWith (+) c 1 l


-- Sorts list of (char, freq) tuples by freq in ascending order
sorted :: CharWeights -> CharWeights
sorted = List.sortBy (compare `on` snd)


-- Creates a Huffman tree given a list of characters and their frequencies
makeTree :: CharWeights -> Node
makeTree = makeTree' . convert . sorted
    where makeTree' [n] = n
          makeTree' (n1:n2:ns)
            = makeTree' $ List.insert (merge n1 n2) ns

          convert = foldr (\(c, w) cs -> Leaf c w : cs) []


-- getEncodings :: tree -> char encoding map
-- Takes a tree and returns a map of all characters and their
-- encodings present in the tree
getEncodings :: Node -> Map.Map Char [Bit]
getEncodings t = Map.fromList $ getEncodings' t []
    where getEncodings' (Leaf c _) bits = [(c, bits)]
          getEncodings' (Inner n1 n2 _) bits
            = (getEncodings' n1 $ bits ++ [Zero]) ++ (getEncodings' n2 $ bits ++ [One])


-- getBits :: tree -> text -> bits
-- Transforms text into a sequence of tree-encoded bits
getBits :: Node -> String -> [Bit]
getBits t = concat . map (\c -> encodings Map.! c)
    where encodings = getEncodings t


-- getString :: tree -> bits -> text
-- Takes a tree-encoded sequence of bits and returns
-- the string that it encodes
getString :: Node -> [Bit] -> String
getString tree bits = getString' tree tree bits ""
    where getString' _ (Leaf c _) [] s = s ++ [c]
          getString' _ _ [] s = s
          getString' r (Leaf c _) b s = getString' r r b (s ++ [c])
          getString' r (Inner left right _) (b:bs) s
            | b == Zero = getString' r left bs s
            | otherwise = getString' r right bs s


-- Converts a sequence of bits into a Word8 number.
-- It is assumed that MSb is the last one last in the list.
bitsToWord :: [Bit] -> Word8
bitsToWord byte = bitsToWord' byte 0
    where bitsToWord' [] acc = acc
          bitsToWord' (b:bs) acc
            | b == Zero = bitsToWord' bs (2*acc)
            | otherwise = bitsToWord' bs (2*acc + 1)


-- Converts a Word8 number into a sequence of bits with
-- MSb as the last bit.
wordToBits :: Word8 -> [Bit]
wordToBits word = wordToBits' word []
    where wordToBits' 0 bits = replicate (wordSize - length bits) Zero ++ bits
          wordToBits' c bits
            | c `mod` 2 == 0 = wordToBits' (c `div` 2) (Zero:bits)
            | otherwise      = wordToBits' (c `div` 2) (One:bits)


-- Extends a list of Bits by Zeros so that its length
-- is divisible by wordSize
extendToMultipleOfWordSize :: [Bit] -> [Bit]
extendToMultipleOfWordSize bits
    | (length bits) `mod` wordSize == 0 = bits
    | otherwise = bits ++ replicate (wordSize - length bits `mod` wordSize) Zero


-- Converts a list of Bits to a ByteString
bitstreamToByteString :: [Bit] -> BS.ByteString
bitstreamToByteString bits = bsts (extendToMultipleOfWordSize bits) []
  where bsts [] ws = BS.pack ws
        bsts b s = bsts (drop wordSize b) (s ++ [bitsToWord $ take wordSize b])


-- encode :: filepath
-- Encodes the file at filepath using Huffman encoding.
-- The encoded file is stored in the file filepath + ".out"
-- The corresponding Huffman tree is stored in the file filepath + ".tree"
encode :: String -> IO ()
encode file = do
    text <- readFile file
    let tree = makeTree $ getFreqList text
    let encoded = bitstreamToByteString $ getBits tree text
    writeFile (file ++ ".tree") (show tree)
    BS.writeFile (file ++ ".out") encoded


-- decode :: filepath -> tree filepath
-- Decodes the file at filepath using the tree stored at tree filepath.
-- The decoded file is stored in the file filepath + ".out"
-- It is assumed that both the tree and the encoded file are valid,
-- otherwise errors will be thrown.
decode :: String -> String -> IO ()
decode file treeFile = do
    tree' <- readFile treeFile
    let tree = read tree'::Node
    text <- BS.readFile file
    let decoded = getString tree $ concat $ map wordToBits $ BS.unpack text
    writeFile (file ++ ".out") decoded
