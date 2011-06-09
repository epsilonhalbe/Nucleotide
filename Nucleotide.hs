module Nucleotide (filterSequences, s2n, isSolution, checkRules, checkRule1, checkRule2) where
import Data.Char (toUpper)
import Data.List (sort)

data Nucleotide = A|B|C|D|H|G|K|M|R deriving (Ord,Eq,Enum,Show)

isSolution :: String -> Bool
isSolution [] = True
isSolution nucs = (checkRules chunk) && (isSolution rest)
    where chunk = stringToNucleotideSequence [n1,n2,n3]
          (n1:n2:n3:rest) = nucs

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

checkRules :: [Nucleotide] -> Bool
checkRules nucs = (checkRule1 nucs) || (checkRule2 nucs)

checkRule1 :: [Nucleotide] -> Bool
checkRule1 nucs = and (zipWith (==) [n'..] nucs')
    where (n:ns) = nucs
          n' = fromEnum n
          nucs' = map fromEnum nucs

checkRule2 :: [Nucleotide] -> Bool
checkRule2 nucs = and (zipWith (==) nucs nucs')
    where nucs' = tail nucs

-- -----------------------------------------------------------------------------

filterSequences :: [Nucleotide] -> [[Nucleotide]]
filterSequences seq = filterSequences' ([],seq)

filterSequences' :: ([[Nucleotide]],[Nucleotide]) -> [[Nucleotide]]
filterSequences' (fseq,[]) = fseq
filterSequences' (fseq,seq)
    | check = filterSequences' (fseq, restseq)
    | otherwise = filterSequences' (fseq ++ [sortseq], restseq)
    where (sseq, restseq) = splitAt 12 seq
          sortseq = sort sseq
          check = (checkRules sseq) && (sortseq `elem` fseq)

sort12Sequence :: [Nucleotide] -> [Nucleotide]
sort12Sequence = concat . sort . (splitevery 3)

splitevery :: Int -> [a] -> [[a]]
splitevery 0 xs = [xs]
splitevery n xs = [x1_xn] ++ (splitevery n rest)
    where (x1_xn, rest) = splitAt n xs
