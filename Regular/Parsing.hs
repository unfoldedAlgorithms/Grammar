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
parse rules ""    = Just []
parse rules input = let

  apply_rules :: [Rule] -> String -> Maybe (Rule, String)
  apply_rules [] _ = Nothing
  apply_rules (r:rs) input =
    case apply_rule r input of
      Nothing   -> apply_rules rs input
      Just rest -> Just (r, rest)
  
  in case apply_rules rules input of
    Nothing -> Nothing
    Just (rule, rest) -> let
      pv = variable_of . pattern_of $ rule
      rest' = case (reduction_of rule) of
        (Re)      -> rest
        (Rt t)    -> rest
        (Rtv t v) -> v ++ rest
      in fmap ((:) rule) $ parse rules rest'
