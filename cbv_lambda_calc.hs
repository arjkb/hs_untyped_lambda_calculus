-- type Variable = String
data Term a = Var a
  | Abstraction (Term a) (Term a)
  | Application (Term a) (Term a)
  deriving (Show)
