module Rule where

import Token
import Utility

{-
  Right-Regular Grammar rule types
    * RuleEmpty    : Variable reduces to epsilon; 
    * RuleTerminal : Variable reduces to Terminal; A -> aB
    * RuleVariable : Variable reduces to Terminal and Variable
-}
data Rule
  = RuleEmpty    Variable                   -- v -> e
  | RuleTerminal Variable Terminal          -- v -> t
  | RuleVariable Variable Terminal Variable -- v -> t v'
  deriving (Show)

{-
  returns Maybe:
    * Either
      * resulting Terminal
      * resulting Terminal and Variable
    * rest of (unparsed) input
-}
apply_rule ::
  Rule -> String ->
  Maybe (Either (Maybe Terminal) (Terminal, Variable), String)
apply_rule rule input =
  case rule of
    (RuleEmpty    v)     ->
      case behead v input of
        Nothing   -> Nothing
        Just rest -> Just (Left Nothing, rest)
    (RuleTerminal v t)   ->
      case behead v input of
        Nothing   -> Nothing
        Just rest -> Just (Left $ Just t, rest)
    (RuleVariable v t v') ->
      case behead v input of
        Nothing   -> Nothing
        Just rest -> Just (Right (t, v'), rest)

  -- case rule of
  --   (RuleEmpty    v)     ->
  --     case behead v input of
  --       Nothing   -> Nothing
  --       Just rest -> Just (Left Nothing, rest)
  --   (RuleTerminal v t)   ->
  --     case behead v input of
  --       Nothing   -> Nothing
  --       Just rest -> Just (Left $ Just t, rest)
  --   (RuleVariable v t v') ->
  --     case behead v input of
  --       Nothing   -> Nothing
  --       Just rest -> Just (Right (t, v'), rest)
