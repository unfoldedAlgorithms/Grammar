module Utility where

behead :: String -> String -> Maybe String
behead target input = let
  helper :: String -> String -> Maybe String
  helper [] _  = Nothing
  helper xs [] = Just xs
  helper (x:xs) (y:ys) =
    if x == y
      then helper xs ys
      else Nothing
  in helper input target

