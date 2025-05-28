--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

data Expr = Var String              -- Variable
          | Str String              -- String literal
          | Boo Bool                -- Boolean literal: `True`, `False`
          | Rev Expr                -- Reverse a sstring
          | Let String Expr Expr    -- let-expression: `let x = e1 in e2`
          | Bin Binop  Expr Expr    -- Binary operation: `e1 ++ e1`, etc
          | Lam String Expr         -- Function definition
          | App Expr   Expr         -- Function Call
    deriving Show                   -- For printing exprs

data Binop = Cat    -- Concatenate strings (++)
           | Eq     -- Equality check (==)
           | And    -- Boolean conjunction (&&)
           | Or     -- Boolean disjunction (||)
    deriving Show

data Value = ValStr String | ValBoo Bool | ValClos String Expr ListEnv
    deriving Show

type ListEnv = [(String, Value)]

data UnboundError = Unbound String
    deriving Show

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

lookupInEnv :: ListEnv -> String -> Either UnboundError Value
lookupInEnv env s = case env of
    [] -> Left (Unbound "variable is unbound")
    ((name, val):xs) -> if (name == s)
                        then (Right val)
                        else lookupInEnv xs s

applyOp :: Binop -> Value -> Value -> Value
applyOp Cat v1 v2 = case (v1, v2) of 
    (ValStr s1, ValStr s2) -> ValStr (s1 ++ s2)
    _                      -> error "type error"
applyOp Eq v1 v2 = case (v1, v2) of
    (ValBoo b1, ValBoo b2) -> ValBoo (b1 == b2)
    (ValStr s1, ValStr s2) -> ValBoo (s1 == s2)
    _                      -> error "type error"
applyOp And v1 v2 = case (v1, v2) of
    (ValBoo b1, ValBoo b2) -> ValBoo (b1 && b2)
    _                      -> error "type error"
applyOp Or v1 v2 = case (v1, v2) of
    (ValBoo b1, ValBoo b2) -> ValBoo (b1 || b2)
    _                      -> error "type error"

eval :: ListEnv -> Expr -> Either UnboundError Value
eval env (Var s)        = lookupInEnv env s 
eval _   (Str s)        = Right (ValStr s)
eval _   (Boo b)        = Right (ValBoo b)
eval env (Rev e)        = case eval env e of
    Right (ValStr s) -> Right (ValStr (reverse s))
    _        -> Left (Unbound "Reverse type error")
eval env (Let s e1 e2)  = case eval env e1 of
    Right v -> eval ((s,v):env) e2
    _       -> Left (Unbound "Let expr type error")
eval env (Bin op e1 e2) = case (eval env e1, eval env e2) of
    (Right v1, Right v2) -> Right (applyOp op v1 v2)
    _                    -> Left (Unbound "Binary operation type error")
eval env (Lam s e)      = Right (ValClos s e env)
eval env (App e1 e2)    = case (eval env e1, eval env e2) of
    (Right (ValClos s e l), Right v) -> eval ((s, v):l) e
    _                                -> Left (Unbound "Funct App type error")

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

exampleAST :: Expr
exampleAST = Let "f" (Lam "s" (Bin Cat (Var "s") (Str " is cute"))) (App (Var "f") (Str "nympha"))

example2 :: Expr
example2 = Let "str" (Str " is cute")
            (Let "f" (Lam "s" (Bin Cat (Var "s") (Var "str")))
                (Let "str" (Str " is smelly")
                    (App (Var "f") (Str "nympha"))))