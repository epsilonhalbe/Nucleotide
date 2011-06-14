import TAP
import Nucleotide
import Rules

-- pastebin.ca/2076873

main = runTests $ do
    planTests 10

    is (isSolution "AAABBBCCCDDD") True $ Just "testing sequence"
    is (isSolution "ABCABCABCABC") True $ Just "testing allthesame"
    is (isSolution "ABCAAABBBBCD") True $ Just "testing sequence and allthesame"
    is (rule_sequence $ s2n "ABC") True $ Just "minimal test sequence"
    is (rule_allthesame $ s2n "AAA") True $ Just "minimal test allthesame"
    is (checkRules $ s2n "AAA") True $ Just "minimal test both1"
    is (checkRules $ s2n "ABC") True $ Just "minimal test both2"
--    isnt (isSolution "AAABBBCCCDD") True $ Just "expect '12 letters'"
--    isnt (isSolution "") True $ Just "test occurence of emptystring"

-- don't have to check for unviable letters -> haskells type checker is powerful
-- note true/false undefined
