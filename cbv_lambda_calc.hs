-- type Variable = String
-- data Variable = Var Char deriving(Show)

import Data.List
replace :: String -> Char -> [Char] -> String
-- replace word ch ch_new = map (\letter -> if letter == ch then ch_new else letter) word
-- replace word ch ch_new = if  Data.List.elem ch word
--   then replace
--   else word
replace [] _ _ = []
replace (x:xs) ch ch_new = if x == ch
  then ch_new ++ (replace xs ch ch_new)
  else x:(replace xs ch ch_new)
-- https://stackoverflow.com/questions/19545253/haskell-replace-characters-in-string

data Term = Var String
  | Lambda Char Term
  | Application Term Term
  deriving (Show, Eq)

-- subst::String -> Term -> Term -> Term
-- [x -> t1] t2
-- replace all free occurences of x in t1 by t2
-- subst s (Lambda a (Var b)) (Var f) = Var x where
--   x = replace b a (f !! 0)


a = Var "xy"
t1 = Lambda 'x' a
t2 = Var "x"
