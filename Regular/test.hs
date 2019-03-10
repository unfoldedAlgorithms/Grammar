import Token
import Rule
import Parsing

rules =
  -- v -> tw
  (map make_rule_v_tv
    [ ("phrase", "henry", "verb-phrase")
    , ("phrase", "john",  "verb-phrase")
    , ("verb-phrase", "hit",    "object-phrase")
    , ("verb-phrase", "kicked", "object-phrase")
    ]) ++
  (map make_rule_s_tv
    [ ("", "phrase")
    ]) ++
  -- v -> t
  (map make_rule_v_t
    [ ("object-phrase", "a ball")
    , ("object-phrase", "the can")
    ]) ++
  (map make_rule_s_t
    [
    ]) ++
  -- v -> e
  (map make_rule_v_e
    [
    ])
  -- ++ make_rule_s_e  

input = "henry kicked the can"


main = do
  let result = parse rules input
  putStrLn $ case result of
    Nothing ->
      "\"" ++ input ++ "\" is ill-formed"
    Just rules ->
      "\"" ++ input ++ "\" is well-formed:\n" ++
      (foldl (\xs ys -> xs ++ "\n    " ++ ys) "" $ map show rules) ++ "\n"

