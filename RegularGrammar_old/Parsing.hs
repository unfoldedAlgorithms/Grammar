module Parsing where

import Token
import Rule

parse :: [Rule] -> String -> [Terminal]
parse rules input = let
  {-
    iterate through rules until one applies,
    then apply that rule.
    if none apply, Nothing.
  -}
  apply_rule_step :: [Rule] -> String ->
    Maybe (Either (Maybe Terminal) (Terminal, Variable), String)
  apply_rule_step [] _ = Nothing
  apply_rule_step (r:rs) input =
    case apply_rule r input of
      Nothing -> apply_rule_step rs input
      Just x  -> Just x
  {-
    apply rule steps until input it empty,
    leaving a trail of parsed Terminals
  -}
  helper :: String -> [Terminal]
  helper [] = []
  helper input = case apply_rule_step rules input of
    Nothing -> error $ "no rule match for:\n-->[" ++ (take 20 input) ++ "]"
    Just (either_mb_t_tv, rest) ->
      case either_mb_t_tv of
        (Left mb_t) ->
          case mb_t of
            Nothing -> helper rest
            Just  t -> t : helper rest
        (Right (t, v)) ->
          t : helper (v ++ rest)
  in helper input
