module Rule where


import Token
import Utility


{- Notation --------------------------------------------------------------------

  e ::= epsilon (empty string)
  v ::= grammar variable (i.e. non-terminal)
  t ::= grammar terminal

-}


{- Right-Regular Grammar Rules -------------------------------------------------

  In general, grammar rules have the form
    <pattern> -> <reduction>

  Right-Regular Grammars have rules only of the following forms:
    * p -> e  (Re)
    * p -> t  (Rt)
    * p -> tv (Rtv)

-}


{- Pattern --------------------------------------------------------------------}


data Pattern
  | Pi Initial
  = Pv Variable

instance Show Pattern where
  show p = show $ case p of
    Pv v -> v
    Pi i -> i

{- Reduction ------------------------------------------------------------------}


data Reduction
  = Re                    -- -> e
  | Rt  Terminal          -- -> t
  | Rtv Terminal Variable -- -> tv

instance Show Reduction where
  show red = case red of
    (Re) -> "e"
    (Rt t) -> t
    (Rtv t v) = show 
  show (Re)      = "e"
  show (Rt  t)   = t
  show (Rtv t v) = show t ++ show v


{- Rule -----------------------------------------------------------------------}

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

{- Rule Set -------------------------------------------------------------------}

newtype RuleSet = RuleSet ()


{- Rule Folding ---------------------------------------------------------------}


{-
  if rule could by applied to produce end of input,
    Just the rest of input before rule-produced part
  else
    Nothing
-}
fold_rule :: Rule -> String -> Maybe String
fold_rule rule input = let
  pat = pattern_of rule
  red = reduction_of rule
  in case red of
    (Re     ) -> Just input
    (Rt  t  ) -> fmap id $ betail t      input
    (Rtv t v) -> fmap id $ betail (t++v) input
