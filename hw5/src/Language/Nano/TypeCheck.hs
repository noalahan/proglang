{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)  
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = parseFile f >>= typeOfExpr

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseString s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TId]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars :: Type -> [TId]
  freeTVars TInt        = []
  freeTVars TBool       = []
  freeTVars (t1 :=> t2) = L.union (freeTVars t1) (freeTVars t2)
  freeTVars (TVar id)   = [id]
  freeTVars (TList t)   = freeTVars t

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars :: Poly -> [TId]
  freeTVars (Mono t)      = freeTVars t
  freeTVars (Forall id t) = L.delete id (freeTVars t)

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars :: TypeEnv -> [TId]
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Look up a variable in a type environment
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend a type environment with a new binding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

-- | Look up a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TId -> Subst -> Type
lookupTVar a []           = TVar a
lookupTVar a ((id, t):xs) = if a == id then t else lookupTVar a xs

-- | Remove a type variable from a substitution
removeTVar :: TId -> Subst -> Subst
removeTVar _ []           = []
removeTVar a ((id, t):xs) = if a == id then removeTVar a xs 
                                       else (id, t):(removeTVar a xs)
     
-- | Things to which type substitutions can be applied
class Substitutable a where
  apply :: Subst -> a -> a
  
-- | Apply substitution to type
instance Substitutable Type where  
  apply :: Subst -> Type -> Type
  apply _   TInt        = TInt
  apply _   TBool       = TBool
  apply sub (t1 :=> t2) = (apply sub t1) :=> (apply sub t2)
  apply sub (TVar id)   = lookupTVar id sub
  apply sub (TList t)   = TList (apply sub t)

-- | Apply substitution to poly-type
instance Substitutable Poly where    
  apply :: Subst -> Poly -> Poly
  apply sub (Mono t)      = Mono (apply sub t)
  apply sub (Forall id t) = Forall id (apply (removeTVar id sub) t)

-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply :: Subst -> Subst -> Subst
  apply sub to = zip keys (map (apply sub) vals)
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply :: Subst -> TypeEnv -> TypeEnv
  apply sub gamma = zip keys (map (apply sub) vals)
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TId -> Type -> Subst
extendSubst sub a t = (a, (apply sub t)):(clean sub)
  where
    clean :: Subst -> Subst
    clean [] = []
    clean ((id, subt):xs) = (id, apply [(a, t)] subt):(clean xs)
      
--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------
      
-- | State of the type inference algorithm      
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving (Eq,Show)

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar ("a" ++ show n)
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TId -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n
        
-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TId -> Type -> InferState
unifyTVar st a (TVar id)                  = if a == id then st else extendState st a (TVar id)
unifyTVar st a t | occurs a (freeTVars t) = throw (Error ("type error: cannot unify " ++ a ++ " and " ++ show t ++ " (occurs check)"))
                 | otherwise              = extendState st a t
  where
    occurs :: TId -> [TId] -> Bool
    occurs _  []     = False
    occurs id (x:xs) = if id == x then True else occurs id xs

-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st (TVar a) t = unifyTVar st a t
unify st t (TVar a) = unifyTVar st a t
unify st TInt t     = if t == TInt then st else throw (Error ("type error: cannot unify Int and " ++ show t))
unify _  t TInt     = throw (Error ("type error: cannot unify " ++ show t ++ " and Int"))
unify st TBool t    = if t == TBool then st else throw (Error ("type error: cannot unify Bool and " ++ show t))
unify _  t TBool    = throw (Error ("type error: cannot unify " ++ show t ++ " and Bool"))
unify st (TList t1) (TList t2) = unify st t1 t2
unify st (arg1 :=> res1) (arg2 :=> res2) = unify st1 res1' res2'
  where
    st1 = unify st arg1 arg2
    res1' = apply (stSub st1) res1
    res2' = apply (stSub st1) res2

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    

infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
infer st gamma (EVar x)        = case (lookupVarType x gamma) of
  Mono t     -> (st, t)
  p          -> (st {stCnt = n}, t)
    where (n, t) = instantiate (stCnt st) p
infer st gamma (ELam x body)   = (st2, apply (stSub st2) arg :=> res)
  where
    arg        = freshTV (stCnt st)
    st1        = st { stCnt = stCnt st + 1 }
    gamma'     = extendTypeEnv x (Mono arg) gamma
    (st2, res) = infer st1 gamma' body
infer st gamma (EApp e1 e2)    = (st4, apply (stSub st4) res)
  where
    (st1, f)   = infer st gamma e1   -- the function
    (st2, arg) = infer st1 gamma e2  -- the argument
    res        = freshTV (stCnt st2) -- the result
    st3        = st2 { stCnt = stCnt st2 + 1 }
    st4        = unify st3 f (arg :=> res)
infer st gamma (ELet x e1 e2)  = infer st3 gamma2 e2
  where
    t = freshTV (stCnt st)
    st1 = st { stCnt = stCnt st + 1}
    gamma1 = extendTypeEnv x (Mono t) gamma
    (st2, t1) = infer st1 gamma1 e1
    st3 = unify st2 t1 (apply (stSub st2) t)
    gamma2    = extendTypeEnv x (generalize gamma1 t1) gamma1
infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2)   = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil            = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
generalize gamma t = wrap ((freeTVars t) L.\\ (freeTVars gamma)) t
  where
    wrap :: [TId] -> Type -> Poly
    wrap [] t = Mono t
    wrap (x:xs) t = Forall x (wrap xs t)
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n s = helper n [] s
  where
    helper :: Int -> Subst -> Poly -> (Int, Type)
    helper n sub (Mono t)     = (n, apply sub t)
    helper n sub (Forall a s) = helper (n + 1) ((a, freshTV n):sub) s
      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono (TInt :=> TInt :=> TInt))
  , ("-",    Mono (TInt :=> TInt :=> TInt))
  , ("*",    Mono (TInt :=> TInt :=> TInt))
  , ("/",    Mono (TInt :=> TInt :=> TInt))
  , ("==",   Forall "a" (Mono ((TVar "a") :=> (TVar "a") :=> TBool)))
  , ("!=",   Forall "a" (Mono ((TVar "a") :=> (TVar "a") :=> TBool)))
  , ("<",    Mono (TInt :=> TInt :=> TBool))
  , ("<=",   Mono (TInt :=> TInt :=> TBool))
  , ("&&",   Mono (TBool :=> TBool :=> TBool))
  , ("||",   Mono (TBool :=> TBool :=> TBool))
  , ("if",   Forall "a" (Mono (TBool :=> (TVar "a") :=> (TVar "a") :=> (TVar "a"))))
  -- lists: 
  , ("[]",   Forall "a" (Mono (TList (TVar "a"))))
  , (":",    Forall "a" (Mono ((TVar "a") :=> (TList (TVar "a")) :=> (TList (TVar "a")))))
  , ("head", Forall "a" (Mono ((TList (TVar "a")) :=> (TVar "a"))))
  , ("tail", Forall "a" (Mono ((TList (TVar "a")) :=> (TList (TVar "a")))))
  ]

