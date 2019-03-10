module Token where

type Terminal = String
type Variable = String

-- for convenience, epsilon is a Terminal.
-- just use epsilon rather than empty string in rules
epsilon = "" :: Terminal
