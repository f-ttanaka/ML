module Error where

import           Control.Monad.Trans (MonadTrans, lift)
import           Syntax
import           Text.Parsec         (ParseError)
import           Typing.Type

data Error = Parse ParseError
  | TypeMismatch Expr Type Type
  | VariableNotBound Name
  | Default String
  | Infinite Name String
  | UnificationFail Type Type
  | CompileError

instance Show Error where
  show (Parse err) = show err
  show (TypeMismatch e ex act) =
    "type mismatch on " ++ show e ++ "\n" ++ "expected: " ++ show ex ++ ", actual: " ++ show act
  show (VariableNotBound x) = "variable not bound: " ++ show x
  show (Default str) = "implementation error on: " ++ str
  show (Infinite x v) = "undecidable: forall " ++ x ++ ". " ++ v
  show (UnificationFail t1 t2) = "unification fail: " ++ show t1 ++ " and " ++ show t2
  show CompileError = "compile error"

type Check = Either Error

throw :: Error -> Check a
throw err = Left err

liftThrow :: MonadTrans t => Error -> t Check a
liftThrow err = lift (throw err)

assertError :: MonadTrans t => Bool -> Error -> t Check ()
assertError True err = liftThrow err
assertError False _  = lift $ return ()
