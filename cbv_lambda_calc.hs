-- type Variable = String
-- data Variable = Var Char deriving(Show)
data Term = Var String
  | Lambda String Term
  | Application Term Term
  deriving (Show)

-- subst::Term -> Term -> Term
-- subst Abstraction a b = (Var b) (Var a)
