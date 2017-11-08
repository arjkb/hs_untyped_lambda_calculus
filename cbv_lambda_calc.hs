import Data.List

data Term = Var String    -- variable
  | Lambda String Term    -- abstraction
  | Application Term Term -- application
  deriving (Show, Eq)

subst :: Term -> Term -> Term -> Term
-- [x -> s]y

-- variable case
subst (Var x) (Var y) s = if x == y
  then s
  else (Var y)

-- application case
subst x (Application t1 t2) s = Application (subst x t1 s) (subst x t2 s)

-- subst x (Lambda y t) s = Lambda y (subst x t s) --problematic

-- this is the capture-avoiding part
-- abstraction case
subst (Var x) (Lambda y t) (Var s) = if s == y
  then Var x  -- INCORRECT!
  else Lambda y (subst (Var x) t (Var s))

isValue :: Term -> Bool
isValue (Lambda _ _) = True
isValue _ = False

x = Var "x"
t = Lambda "z" x
yt = Lambda "y" x
y = Var "y"

appl = Application t yt


eval1 :: Term -> Maybe Term

-- E_APPABS: (Lx.t)v -> [x->v]t
eval1 (Application (Lambda x t) v2) = if isValue v2
  then Just (subst (Var x) t v2)
  else Nothing

eval1 (Application t1 t2) = if isValue t1
  then case (eval1 t2) of   -- E_APP2
    Just t -> Just (Application t1 t)
    otherwise -> Nothing
  else case (eval1 t1) of   -- E_APP1
    Just t -> Just (Application t t2)
    otherwise -> Nothing

eval1 _ = Nothing


eval :: Term -> Term
eval t = case (eval1 t) of
  Just t' -> eval t'
  Nothing -> t
-- x = Var "x"
-- xx = Var "x"
-- y = Var "y"
-- z = Lambda "K" (Var "L")

a = Lambda "x" (Var "x")
aa = Lambda "x" (Var "g")
b = Var "y"
