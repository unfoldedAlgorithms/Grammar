module Parsing where

parse :: [Rule] -> String -> [Terminal]
parse rules input = let
  helper :: String -> [Terminal]
  helper [] = []
  helper 
  in 
