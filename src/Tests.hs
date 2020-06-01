import Huffman

main :: IO ()
main = do
    let textFile = "tests/example.txt"
    encode textFile
    putStrLn "Encoded the example file."
    let binFile = "tests/example.bin"
    let treeFile = "tests/example.tree"
    decode binFile treeFile
    putStrLn "Decoded the example file."
