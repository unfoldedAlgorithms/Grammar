module Token where

{- Variable -------------------------------------------------------------------}


data Variable = S | V String

instance Show Variable where
  show (S  ) = "S"
  show (V s) = s -- "<"++s++">"

string_of (S  ) = "S"
string_of (V s) = s


{- Terminal --- ---------------------------------------------------------------}


data Terminal = T String

instance Show Terminal where
  show (T s) = s
