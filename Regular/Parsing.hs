module Parsing where

import Token
import Rule

{-
  given a list of Rules `rules` and a String `input`,
  if input is in the Lang(rules),
    Just the sequence of rule applications to produce the input
  otherwise
    Nothing
-}
parse :: [Rule] -> String -> Maybe [Rule]
parse rules "S"   = Just []
parse rules input = let

  fold_rules :: [Rule] -> String -> Maybe (Rule, String)
  fold_rules [] _ = Nothing
  fold_rules (r:rs) input =
    case fold_rule r input of
      Nothing   -> fold_rules rs input
      Just rest -> Just (r, rest)
  
  in case fold_rules rules input of
    Nothing -> Nothing
    Just (rule, rest) -> let
      rest' = rest ++ (string_of . variable_of . pattern_of $ rule)
      in fmap ((:) rule) $ parse rules rest'
