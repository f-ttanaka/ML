module Typing.Infer where

import           Control.Monad           (foldM, replicateM)
import           Control.Monad.Trans.RWS
import           Data.Foldable           (Foldable (..))
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Env                     as E
import           Error
import           Syntax
import           Typing.Type

type TEnv = E.Env Scheme
type Count = Int
type Infer a = RWST TEnv () Count Check a

typeVars :: [String]
typeVars = do
  n <- [1..]
  replicateM n ['a'..'z']

freshVar :: Infer Type
freshVar = do
  c <- get
  put (c+1)
  return $ TVar (typeVars !! c)

instanciate :: Scheme -> Infer Type
instanciate (Forall xs t) = do
  let tvs = S.toList xs
  tvs' <- mapM (const freshVar) tvs
  let sub = M.fromList (zip tvs tvs')
  return (apply sub t)

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  let xs = S.difference (ftv t) (ftv env)
  return (Forall xs t)

inferPrim :: [Expr] -> Type -> Infer (Type, Subst)
inferPrim args expected = do
  tv <- freshVar
  (tf, sub1) <- foldM inferStep (id, mempty) args
  sub2 <- unify (apply sub1 (tf tv)) expected
  return (apply sub2 tv, sub2 `compose` sub1)
  where
    inferStep :: (Type -> Type, Subst) -> Expr -> Infer (Type -> Type, Subst)
    inferStep (tf,sub) e = do
      (t,sub') <- local (apply sub) (infer e)
      return (tf . TArrow t, sub' `compose` sub)

typeOfBinOp :: BinOp -> Type
typeOfBinOp Lt = TInt `TArrow` (TInt `TArrow` TBool)
typeOfBinOp Eq = TInt `TArrow` (TInt `TArrow` TBool)
typeOfBinOp _  = TInt `TArrow` (TInt `TArrow` TInt)

inEnv :: [(Name,Scheme)] -> Infer a -> Infer a
inEnv bs m = local scope m
  where
    scope :: TEnv -> TEnv
    scope env = foldr' (\(x,sc) e -> M.insert x sc e) env bs

lookupTEnv :: Name -> Infer Type
lookupTEnv x = do
  env <- ask
  case M.lookup x env of
    Nothing -> liftThrow (VariableNotBound x)
    Just sc -> instanciate sc

infer :: Expr -> Infer (Type, Subst)
infer LInt{}           = return (TInt, mempty)
infer LBool{}          = return (TBool, mempty)
infer (Var x) = (,) <$> lookupTEnv x <*> return mempty
infer (BinExpr op e1 e2) = inferPrim [e1,e2] (typeOfBinOp op)
infer (IfExpr e1 e2 e3) = do
  tv <- freshVar
  inferPrim [e1,e2,e3] (TBool `TArrow` (tv `TArrow` (tv `TArrow` tv)))
infer (Lambda x e) = do
  tv <- freshVar
  (tBody,sub) <- local (M.insert x (Forall mempty tv)) (infer e)
  return (apply sub tv `TArrow` tBody, sub)
infer (App e1 e2) = do
  tv <- freshVar
  (t1,sub1) <- infer e1
  (t2,sub2) <- local (apply sub1) (infer e2)
  sub3 <- unify (apply sub2 t1) (t2 `TArrow` tv)
  return (apply sub3 tv, sub3 `compose` sub2 `compose` sub1)
infer (LetExpr binds body) = do
  (bs,sub) <- inferBinds binds
  let (xs,scs) = unzip bs
  inEnv (zip xs (apply sub scs)) (infer body)
infer (LetRecExpr binds body) = do
  (bs,sub) <- inferRecBinds binds
  let (fs,scs) = unzip bs
  inEnv (zip fs (apply sub scs)) (infer body)

inferBind :: ([Scheme], Subst) -> Expr -> Infer ([Scheme], Subst)
inferBind (scs,sub) e = do
  (t,sub') <- local (apply sub) (infer e)
  sc <- generalize t
  return (scs ++ [sc], sub' `compose` sub)

inferBinds :: [(Name,Expr)] -> Infer ([(Name,Scheme)], Subst)
inferBinds binds = do
  let (xs,es) = unzip binds
  (scs,sub) <- foldM inferBind mempty es
  return (zip xs (apply sub scs), sub)

inferRecBinds :: [(Name,Name,Expr)] -> Infer ([(Name,Scheme)], Subst)
inferRecBinds binds = do
  let (fs,xs,es) = unzip3 binds
      n = length binds
  tvs1 <- replicateM n freshVar
  tvs2 <- replicateM n freshVar
  let fBinds = [(f, Forall mempty (tv1 `TArrow` tv2)) | (f, tv1, tv2) <- zip3 fs tvs1 tvs2]
      xBinds = [(x, Forall mempty tv1) | (x,tv1) <- zip xs tvs1]
  (_,sub) <- inEnv (fBinds ++ xBinds) (foldM inferBind mempty es)
  return ([(f, apply sub t) | (f,t) <- fBinds], sub)

runInfer :: TEnv -> Expr -> Check Type
runInfer env expr = case runRWST (infer expr) env 0 of
  Right ((t,_),_, _) -> return t
  Left err           -> throw err

runInferBinds :: TEnv -> [(Name,Expr)] -> Check [(Name,Scheme)]
runInferBinds env binds = case runRWST (inferBinds binds) env 0 of
  Right ((bs,_),_,_) -> return bs
  Left err           -> throw err

runInferRecBinds :: TEnv -> [(Name,Name,Expr)] -> Check [(Name,Scheme)]
runInferRecBinds env binds = case runRWST (inferRecBinds binds) env 0 of
  Right ((bs,_),_,_) -> return bs
  Left err           -> throw err

type VarCount = Int
type Subst = M.Map Name Type
type Equation = (Type, Type)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set Name

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply sub (x,y) = (apply sub x, apply sub y)
  ftv (x,y) = S.union (ftv x) (ftv y)

instance Substitutable a => Substitutable [a] where
  apply sub xs = [apply sub x | x <- xs]
  ftv xs = S.unions [ftv x | x <- xs]

instance Substitutable Type where
  apply sub t@(TVar x)     = M.findWithDefault t x sub
  apply sub (TArrow t1 t2) = TArrow (apply sub t1) (apply sub t2)
  apply _ ty               = ty
  ftv (TVar x)       = S.singleton x
  ftv (TArrow t1 t2) = S.union (ftv t1) (ftv t2)
  ftv _              = mempty

instance Substitutable Scheme where
  apply sub (Forall xs t) = Forall xs (apply sub' t)
    where
      sub' = foldr' M.delete sub xs
  ftv (Forall xs t) = S.difference (ftv t) xs

instance Substitutable a => Substitutable (M.Map v a) where
  apply sub m = M.map (apply sub) m
  ftv env = S.unions [ftv x | x <- M.elems env]

compose :: Subst -> Subst -> Subst
compose sub1 sub2 = M.union (M.map (apply sub1) sub2) sub1

occurs :: Name -> Type -> Bool
occurs x (TVar y)       = x == y
occurs x (TArrow t1 t2) = occurs x t1 || occurs x t2
occurs _ _              = False

solve :: [Equation] -> Subst -> Maybe Subst
solve [] sub = Just sub
solve ((t1,t2):eqs) sub | t1 == t2 = solve eqs sub
solve ((TArrow t1 t2, TArrow t1' t2'):eqs) sub = solve ((t1,t1'):(t2,t2'):eqs) sub
solve ((TVar x, t2):eqs) sub
  | occurs x t2 = Nothing
  | otherwise = let sub' = M.fromList [(x,t2)] in
      solve (apply sub' eqs) (compose sub' sub)
solve ((t1, TVar y):eqs) sub = solve ((TVar y, t1):eqs) sub
solve _ _ = Nothing

unify :: Type -> Type -> Infer Subst
unify t1 t2 = case solve [(t1,t2)] mempty of
  Just sub -> return sub
  _        -> liftThrow (UnificationFail t1 t2)
