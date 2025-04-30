--------------------------------------------------------------------------------
-- | A data type for arithmetic expressions ------------------------------------
--------------------------------------------------------------------------------
data Expr
  = VarX
  | VarY
  | Sine    Expr
  | Cosine  Expr
  | Average Expr Expr
  | Times   Expr Expr
  | Thresh  Expr Expr Expr Expr
  deriving (Show)

-- e ::= x
--     | y
--     | sin(pi*e)     -- the "sinpi" function
--     | cos(pi*e)     -- the "cospi" function
--     | ((e+e)/2)     -- average the val of 2 exprs
--     | e*e           -- multiply 2 exprs

depth :: Expr -> Int
depth VarX = 0
depth VarY = 0
depth (Sine e) = 1 + depth e
depth (Cosine e) = 1 + depth e
depth (Average e1 e2) = 1 + depth e1 + depth e2
depth (Times e1 e2) = 1 + depth e1 + depth e2
depth (Thresh e1 e2 e3 e4) = 1 + depth e1 + depth e2 + depth e3 + depth e4


-- An expression of depth d is any expression for which all subexpressions have depth of at most d-1:

-- The expressions VarX and VarY both have depth 0.
-- The depth of the expression Sine e is 1 + the depth of e.
-- The depth of the expression Cosine e is 1 + the depth of e.
-- The depth of the expression Average e1 e2 is 1 + the maximum of the depths of e1 and e2.
-- The depth of the expression Times e1 e2 is 1 + the maximum of the depths of e1 and e2.
-- The depth of the expression Thresh e1 e2 e3 e4 is 1 + the maximum of the depths of e1, e2, e3, and e4.
-- For example:

-- the expression Sine VarX has depth 1.
-- the expression Sine (Average VarX VarY) has depth 2.
-- the expression Average (Sine VarX) VarY has depth 2.
-- the expression Thresh VarX VarY (Times VarX (Sine VarY)) has depth 3.

-- let myExpr = ((x+y)/2)*cos(pi*sin(pi*y))
myExpr :: Expr
myExpr = Times (Average VarX VarY) (Cosine (Sine VarY))

