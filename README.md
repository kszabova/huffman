# Huffman

Huffman coding is a simple text compression algorithm. The main idea is that characters that
occur many times in the text are encoded with fewer bits than uncommon characters, which
results in a smaller file size.

## Huffman encoding

The algorithm first parses the entire text and computes charater frequencies. Afterwards,
it builds a binary tree with characters as leaves so that the path from the root to the
leaves is shortest for the most frequent characters and longest for the least frequent
ones. Then it parses the entire file again, now encoding every character as the path
from the root of the tree to the corresponding leaf.

Decompression is done in a reverse way. The compressed text is a string of bits. The
algorithm starts in the root and follows the directions (i.e. bits) until it gets
into a leaf. It then returns the corresponding character and returns to the root.

## Project description

The module exports two functions, `encode :: String -> IO ()` and `decode :: String -> String -> IO ()`.

`encode` has as a parameter the path to the file that is to be compressed. The encoded file
is stored in the file of the same name with the `.out` extension. Its corresponding tree
(which is needed for decompression) is stored in a file of the same name with the `.tree`
extension.

`decode` takes two parameters: path to the compressed file and path to the tree file.
The decompressed file is stored in a file with the same name as the compressed one
with the `.out` extension.

Additionally, the project contains the executable `tests`. It can be run with `stack exec tests`.
The executable encodes the file `tests/example.txt` and decodes the file `tests/example.bin`
with the tree `example.bin`. The files are actually images of each other, so the correctness
of the program can be seen by checking that the text file (binary file) is equal to the
decoded binary (encoded text) file.

## Limitations

Only files that fit in the memory can be compressed or decompressed.

Due to the way the algorithm works, the decompressed file may have extra characters
at the end that were not in the original file. This is caused by the fact that trailing
zeros might need to be added at the end of the bit string as it is only possible
to write entire bytes. The added zeros can encode a path to a character.

## Installation

To set up the module locally, navigate to the project directory and run the following commands:

    stack setup
    stack build

Run ghci with `stack ghci`. This enables the module to be imported.

In the prompt, type `:m Huffman`. The module will now work as expected.

## Example usage

### Encoding a file

`encode "file.txt"`

The compressed file will be stored in `file.txt.out` and the corresponding
tree will be in `file.txt.tree`.

### Decoding a file

`decode "file.bin" "file.tree"`

The decompressed file will be stored in `file.bin.out`.
