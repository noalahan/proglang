-- data Either a b = Left a | Right b

caseExample :: Bool -> String -> String
caseExample = \x y -> case (not x, "hello, " ++ y) of
    (False, s) -> s
    (True, _) -> "the first argument must have been false"

data Expr = Var String
        | Str String
        | Cat Expr Expr
        | Rev Expr
    deriving Show

type ListEnv = [(String, String)]

data UnboundError = Unbound String
    deriving Show

lookupInEnv :: ListEnv -> String -> Either UnboundError String
lookupInEnv env s = case env of
    [] -> Left (Unbound "variable is unbound")
    ((name, val):xs) -> if (name == s)
                        then (Right val)
                        else lookupInEnv xs s

eval :: ListEnv -> Expr -> Either UnboundError String
eval env (Var s) = lookupInEnv env s 
eval env (Str s) = (Right s)
eval env (Cat a b) = ((eval a) ++ (eval b))
eval env (Rev s) = (reverse (eval s))