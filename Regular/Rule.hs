module Rule where

import Utility
import Token

{- Notation --------------------------------------------------------------------

  p ::= grammar rule pattern
  s ::= initial grammar variable
  v ::= intermediate grammar variable (i.e. non-terminal)
  t ::= grammar terminal
  ε ::= epsilon (empty string)

-}


{- Right-Regular Grammar Rules -------------------------------------------------

  In general, grammar rules have the form
    pattern -> reduction

  Right-Regular Grammars have rules only of the following forms:
    * p -> t
    * p -> v
    * p -> tv

-}

{- Pattern --------------------------------------------------------------------}

data Pattern
  = Pv Variable

instance Show Pattern where
  show (Pv v) = show v

variable_of (Pv v) = v

{- Reduction ------------------------------------------------------------------}

data Reduction
  = Rt  Terminal
  | Rv  Variable
  | Rtv Terminal Variable

instance Show Reduction where
  show red = case red of
    (Rt t)    -> show t
    (Rv v)    -> show v
    (Rtv t v) -> show t ++ show v

{- Rule -----------------------------------------------------------------------}

newtype Rule = Rule (Pattern, Reduction)

instance Show Rule where
  show (Rule (pat, red)) = show pat ++ "::=" ++ show red

pattern_of   (Rule (pat, red)) = pat
reduction_of (Rule (pat, red)) = red

{- Rule Making ----------------------------------------------------------------}

make_rule_v_t (v, t) = Rule (Pv (V v), Rt (T t))
make_rule_s_t (   t) = Rule (Pv (S  ), Rt (T t))

make_rule_v_v (v, w) = Rule (Pv (V v), Rv (V w))
make_rule_s_v (   w) = Rule (Pv (S  ), Rv (V w))

make_rule_v_tv (v, t, w) = Rule (Pv (V v), Rtv (T t) (V w))
make_rule_s_tv (   t, w) = Rule (Pv (S  ), Rtv (T t) (V w))

{- Rule Folding ---------------------------------------------------------------}

{-
  if rule could by applied to produce end of input,
    Just the rest of input before rule-produced part
  else
    Nothing
-}
fold_rule :: Rule -> String -> Maybe String
fold_rule rule input = let
  in case (reduction_of rule) of
    (Rt  (T t)  ) -> fmap id $ betail (t               ) input
    (Rv        v) -> fmap id $ betail (     string_of v) input
    (Rtv (T t) v) -> fmap id $ betail (t ++ string_of v) input
