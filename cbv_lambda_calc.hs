-- type Variable = String
data Term a = Var a
  | Abstraction String (Term a)
  | Application (Term a) (Term a)
  deriving (Show)
