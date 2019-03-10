module Parsing where

import Token
import Rule

{-
  given a list of Rules `rules` and a String `input`,
  if input is in the Lang(rules),
    Just the sequence of rule applications to produce the input
  otherwise
    Nothing
-}
parse :: [Rule] -> String -> IO (Maybe [Rule])
parse rules "S"   = return $ Just []
parse rules input = do
  let mb_result = fold_rules rules input
  -- print mb_result
  case mb_result of
    Nothing -> return Nothing
    Just (rule, rest) -> do
      let rest' = rest ++ (string_of . variable_of . pattern_of $ rule)
      -- print rest'
      next_parsed <- parse rules rest'
      return $ fmap ((:) rule) next_parsed

fold_rules :: [Rule] -> String -> Maybe (Rule, String)
fold_rules [] _ = Nothing
fold_rules (r:rs) input =
  case fold_rule r input of
    Nothing   -> fold_rules rs input
    Just rest -> Just (r, rest)
