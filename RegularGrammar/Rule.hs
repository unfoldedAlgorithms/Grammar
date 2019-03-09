module Rule where

import Token
import Utility

data Rule
  = RuleEmpty    Variable                   -- A -> a
  | RuleTerminal Variable Terminal          -- A -> aB
  | RuleContinue Variable Terminal Variable -- A -> e
  deriving (Show)

{- returns Maybe:
    * resulting terminal
    * resulting variable
    * rest of (unparsed) input
-}
apply_rule ::
  Rule -> String ->
  Maybe (Maybe Terminal, Maybe Variable, String)
apply_rule rule input =
  case rule of
    (RuleEmpty    x)     ->
      case behead input x of
        Nothing   -> Nothing
        Just rest -> Just (Just epsilon, Nothing, rest)
    (RuleTerminal x a)   ->
      case behead input x of
        Nothing   -> Nothing
        Just rest -> Just (Just a, Nothing, rest)
    (RuleContinue x a y) ->
      case behead input x of
        Nothing   -> Nothing
        Just rest -> Just (Just a, Just y, rest)
