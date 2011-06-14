module Data (Nucleotide, s2n) where

import Data.Char (toUpper)

data Nucleotide = A|B|C|D|H|G|K|M|R deriving (Ord,Eq,Enum,Show)

charToNucleotide :: Char -> Nucleotide
charToNucleotide 'A' = A
charToNucleotide 'B' = B
charToNucleotide 'C' = C
charToNucleotide 'D' = D
charToNucleotide 'H' = H
charToNucleotide 'G' = G
charToNucleotide 'K' = K
charToNucleotide 'M' = M
charToNucleotide 'R' = R
charToNucleotide _ = error "Unviable letters in sequence"

s2n :: String -> [Nucleotide]
s2n = stringToNucleotideSequence

stringToNucleotideSequence :: String -> [Nucleotide]
stringToNucleotideSequence = map (charToNucleotide . toUpper)
