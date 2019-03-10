import Token
import Rule
import Parsing

rules =
  (map make_rule_e
    [ "hello"
    , "world"
    ]) ++
  (map make_rule_t
    [
    ]) ++
  (map make_rule_tv
    [ ("hello", "there", "World")
    ])

rules =
  [ (Pv "hello" , Rtv "there" "world")
  , (Pv "world" , Re)
  , (Pv " "     , Re) ]

input = "Hello World"

main = print $ parse rules input

