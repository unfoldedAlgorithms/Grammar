module Utility where

{-
  if input begins with target,
    Just rest of input after target
  else
    Nothing
-}
behead :: String -> String -> Maybe String
behead target input = let
  helper :: String -> String -> Maybe String
  helper xs [] = Just xs
  helper [] _  = Nothing
  helper (x:xs) (y:ys) =
    if x == y
      then helper xs ys
      else Nothing
  in helper input target

{-
  if input ends with target,
      Just rest of input before target
  else
    Nothing
-}
betail :: String -> String -> Maybe String
betail target input = fmap reverse
  $ behead (reverse target) (reverse input)
