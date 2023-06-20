{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}

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
  freeTVars :: a -> [TVar]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars (TVar v)     = [v]
  freeTVars (t1 :=> t2)  = L.nub (freeTVars t1 ++ freeTVars t2)
  freeTVars (TList t)    = freeTVars t
  freeTVars _            = []

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars (Mono t)       = freeTVars t          -- mono type
  freeTVars (Forall var p) = (freeTVars p) L.\\ [var]   -- poly type

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Lookup a variable in the type environment  
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend the type environment with a new biding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

-- | Lookup a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TVar -> Subst -> Type
lookupTVar a []            = TVar a
lookupTVar a ((var, t):xs) = if a == var
                             then t
                             else lookupTVar a xs

-- | Remove a type variable from a substitution
removeTVar :: TVar -> Subst -> Subst
removeTVar a []              = []
removeTVar a ((var, val):xs) = if a == var
                               then xs
                               else (var, val) : removeTVar a xs 
     
-- | Things to which type substitutions can be apply
class Substitutable a where
  apply :: Subst -> a -> a
  
-- | Apply substitution to type
instance Substitutable Type where 
  apply sub (TVar v)            = lookupTVar v sub
  apply sub (TList v)           = TList (apply sub v)
  apply sub (t1 :=> t2)         = (apply sub t1) :=> (apply sub t2)
  apply sub t                   = t
  

-- | Apply substitution to poly-type
instance Substitutable Poly where    
  apply [] p               = p
  apply sub (Mono t)       = Mono (apply sub t)
  apply sub (Forall var p) = Forall var $ apply newSub p
                               where newSub = removeTVar var sub
  apply _ p                = p


-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply sub to = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply sub gamma = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TVar -> Type -> Subst
extendSubst sub a t = let bind = [(a,t)] in
                          (apply sub bind) ++ (apply bind sub)

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
freshTV n = TVar $ "a" ++ show n      
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TVar -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n
        
-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TVar -> Type -> InferState
unifyTVar st a t = case (a, t) of (a, TVar v)  -> if a == v
                                                  then st
                                                  else extendState st a t
                                  (a, TList ts) -> let (InferState sub n) = unifyTVar st a ts in
                                                      if sub == []
                                                      then throw ( Error "cannot unify")
                                                      else extendState st a (TList ts)
                                  (a, TBool)    -> extendState st a (TBool)
                                  (a, TInt)     -> extendState st a (TInt)
                                  (a, (t1 :=> t2))       -> throw ( Error "type error")
    
-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st t1 t2 = case (t1, t2) of (TInt, TInt)   -> st
                                  (TInt, _)      -> throw ( Error "type error")
                                  (TBool, TBool) -> st
                                  (TBool, _)     -> throw ( Error "type error")
                                  (TVar v, t)    -> unifyTVar st v t
                                  (f1 :=> r1, f2 :=> r2) -> throw ( Error "cannot unify")
                                  (_, f2 :=> r2) -> throw ( Error "cannot unify")
                                  (f1 :=> r1, _) -> throw ( Error "cannot unify")
                                  

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    
  
infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
infer st gamma (EVar x)        = error "TBD"
infer st gamma (ELam x body)   = error "TBD: infer ELam"
infer st gamma (EApp e1 e2)    = error "TBD: infer EApp"
infer st gamma (ELet x e1 e2)  = error "TBD: infer ELet"
infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
generalize gamma t = error "TBD: generalize"
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n s = error "TBD: instantiate"
      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono $ TInt :=> TInt :=> TInt)
  , ("-",    error "TBD: -")
  , ("*",    error "TBD: *")
  , ("/",    error "TBD: /")
  , ("==",   error "TBD: ==")
  , ("!=",   error "TBD: !=")
  , ("<",    error "TBD: <")
  , ("<=",   error "TBD: <=")
  , ("&&",   error "TBD: &&")
  , ("||",   error "TBD: ||")
  , ("if",   error "TBD: if")
  -- lists: 
  , ("[]",   error "TBD: []")
  , (":",    error "TBD: :")
  , ("head", error "TBD: head")
  , ("tail", error "TBD: tail")
  ]
