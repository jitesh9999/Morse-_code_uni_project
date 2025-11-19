# Morse Code Encoder/Decoder in Haskell

A comprehensive Haskell implementation of Morse code encoding and decoding with advanced tree-based data structures.

## Features
- ✅ Encode text to Morse code using lookup tables
- ✅ Decode Morse code back to text
- ✅ Tree-based encoding/decoding using binary trees
- ✅ Bidirectional conversion between tables and trees
- ✅ Bracket matching parser with custom tree structures

## Usage
```haskell
ghci Assignment2.hs

-- Encode text
ghci> encodeText morseTable "HELLO"

-- Decode morse code
ghci> decodeText morseTable (encodeText morseTable "HELLO")
"HELLO"

-- Tree-based decoding
ghci> let tree = ramify morseTable
ghci> decodeTextWithTree tree (encodeText morseTable "SOS")
"SOS"

-- Check well-bracketed strings
ghci> isWellBracketed "(()())"
True
```
## Technologies
- Haskell
- Functional Programming
- Recursive Data Structures
- Pattern Matching
- Binary Trees

## Files
- Assignment2.hs - Main implementation
- Types.hs - Type definitions and morse code table


