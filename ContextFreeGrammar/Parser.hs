module Parser (make_parser) where

import Rules

type Parser = [Rule] -> String -> [Terminal]

make_parser :: 
