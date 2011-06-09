import TAP
import Nucleotide

-- pastebin.ca/2076873

main = runTests $ do
    planTests 10

    is (isSolution "AAABBBCCCDDD") True $ Just "testing rule1"
    is (isSolution "ABCABCABCABC") True $ Just "testing rule2"
    is (isSolution "ABCAAABBBBCD") True $ Just "testing rule1 and rule2"
    is (checkRule1 $ s2n "ABC") True $ Just "minimal test rule1"
    is (checkRule2 $ s2n "AAA") True $ Just "minimal test rule2"
    is (checkRules $ s2n "AAA") True $ Just "minimal test both1"
    is (checkRules $ s2n "ABC") True $ Just "minimal test both2"
--    isnt (isSolution "AAABBBCCCDD") True $ Just "expect '12 letters'"
--    isnt (isSolution "") True $ Just "test occurence of emptystring"

-- don't have to check for unviable letters -> haskells type checker is powerful
-- note true/false undefined
