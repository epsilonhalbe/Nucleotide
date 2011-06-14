{-# LANGUAGE TemplateHaskell #-}

module Rules where
import Language.Haskell.Extract
import Data

-- checkRules :: [Nucleotide] -> Bool
-- checkRules nucs = (checkRule1 nucs) || (checkRule2 nucs)

checkRules :: [Nucleotide] -> Bool
checkRules nucs = or (map ($ nucs) listOfRules)
    where listOfRules = map snd $(functionExtractor "^rule")

rule_sequence :: [Nucleotide] -> Bool
rule_sequence nucs = and (zipWith (==) [n'..] nucs')
    where (n:ns) = nucs
          n' = fromEnum n
          nucs' = map fromEnum nucs

rule_allthesame :: [Nucleotide] -> Bool
rule_allthesame nucs = and (zipWith (==) nucs nucs')
    where nucs' = tail nucs


