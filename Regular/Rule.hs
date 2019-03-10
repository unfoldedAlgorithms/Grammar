module Rule where

import Token
import Utility

{- Notation --------------------------------------------------------------------

  e ::= epsilon (empty string)
  v ::= grammar variable (i.e. non-terminal)
  t ::= grammar terminal

-------------------------------------------------------------------------------}

{- Right-Regular Grammar Rules -------------------------------------------------

  In general, grammar rules have the form
    <pattern> -> <reduction>

  Right-Regular Grammars have rules only of the following forms:
    * p -> e  (Re)
    * p -> t  (Rt)
    * p -> tv (Rtv)

-------------------------------------------------------------------------------}

data Pattern
  = Pv Variable

instance Show Pattern where
  show (Pv v) = v

variable_of :: Pattern -> Variable
variable_of (Pv v) = v

data Reduction
  = Re                    -- -> e
  | Rt  Terminal          -- -> t
  | Rtv Terminal Variable -- -> tv

instance Show Reduction where
  show (Re)      = "e"
  show (Rt  t)   = t
  show (Rtv t v) = t ++ v

newtype Rule = Rule (Pattern, Reduction)

instance Show Rule where
  show (Rule (pat, red)) = show pat ++ " -> " ++ show red

make_rule_e  (pv      ) = Rule (Pv pv, Re)
make_rule_t  (pv, t   ) = Rule (Pv pv, Rt t)
make_rule_tv (pv, t, v) = Rule (Pv pv, Rtv t v)

pattern_of :: Rule -> Pattern
pattern_of (Rule (pat, _)) = pat

reduction_of :: Rule -> Reduction
reduction_of (Rule (_, red)) = red

{-
  if rule could by applied to produce end of input,
    Just the rest of input before rule-produced part
  else
    Nothing
-}
apply_rule :: Rule -> String -> Maybe String
apply_rule rule input = let
  pat = pattern_of rule
  red = reduction_of rule
  in case red of
    (Re     ) -> Just input_rev
    (Rt  t  ) -> fmap id $ betail t      input
    (Rtv t v) -> fmap id $ betail (t++v) input
