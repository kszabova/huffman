module Huffman where

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

weight :: Node -> Int
weight (Leaf _ w)    = w
weight (Inner _ _ w) = w

merge :: Node -> Node -> Node
merge n1 n2 = Inner n1 n2 (weight n1 + weight n2)
