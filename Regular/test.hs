import Token
import Rule
import Parsing

rules =
  [ make_rule_v_t  ("A", "a"     )
  , make_rule_v_t  ("B", "b"     )
  , make_rule_s_v  (          "AB")
  ]

input = "ab"

main = do
  result <- parse (reverse rules) input
  putStrLn $ case result of
    Nothing ->
      "\"" ++ input ++ "\" is ill-formed"
    Just rs ->
      "\"" ++ input ++ "\" is well-formed:\n" ++
      (foldl (\xs ys -> xs ++ "\n    " ++ ys) ""
        $ map show (reverse rs)) ++ "\n"
