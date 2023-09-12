module Env where

import           Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Data.Map                   as M
import           Error
import           Syntax

type Env a = M.Map Name a

bindVars :: [(Name, a)] -> Env a -> Env a
bindVars bs env = M.union (M.fromList bs) env

lookupVar :: Name -> ReaderT (Env a) Check a
lookupVar x = do
  env <- ask
  case M.lookup x env of
    Just v -> return v
    _      -> liftThrow (VariableNotBound x)
