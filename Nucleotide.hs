module Nucleotide (filterSequences, s2n, isSolution) where
import Data.List (sort)
import Data
import Rules

isSolution :: String -> Bool
isSolution [] = True
isSolution nucs = (checkRules chunk) && (isSolution rest)
    where chunk = s2n [n1,n2,n3]
          (n1:n2:n3:rest) = nucs

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
