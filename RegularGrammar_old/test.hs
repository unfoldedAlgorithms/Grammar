import Parsing
import Rule
import Token

-- rules =
--   [ RuleEmpty    " " ]

rules =
  [
    RuleEmpty " "
  --   RuleVariable "  " "" " "
  -- , RuleTerminal " " " "
  
  , RuleTerminal "1 + 1" "2"
  ]

input = "1 + 1"

main = do
  -- print $ apply_rule (rules !! 0) input
  -- input <- getLine
  let parsed = parse rules input
  putStrLn $ take 20 $ repeat '-'
  print parsed
  putStrLn $ take 20 $ repeat '-'
