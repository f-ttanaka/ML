module REPL.REPL (runREPL) where

import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State
import           Env
import           Error
import           Eval.TreeWalk
import           Eval.ZAM.Compile
import           Parser.Parser             (parseCommand, parseExpr, parseFile,
                                            parseStmt)
import           REPL.Prompt
import           Syntax
import           Typing.Infer

type REPL a = StateT (VEnv,TEnv) IO a

outputResult :: Show a => Check a -> REPL ()
outputResult (Left err)  = lift (print err)
outputResult (Right res) = lift (print res)

-- 1行ずつの文として評価する
evalStmt :: Stmt -> REPL ()
evalStmt (ExprStmt ex) = do
  (vEnv,tEnv) <- get
  case runInfer tEnv ex of
    Left err -> lift (print err)
    _        -> case runEval vEnv ex of
      Left err  -> lift (print err)
      Right res -> lift (putStrLn $ show ex ++ " = " ++ show res)
evalStmt (LetStmt bs) = do
  (vEnv, tEnv) <- get
  let (xs,es) = unzip bs
  case runInferBinds tEnv bs of
    Left err -> lift (print err)
    Right bs' -> case mapM (runEval vEnv) es  of
      Left err -> lift (print err)
      Right vs ->
        let vEnv' = bindVars (zip xs vs) vEnv
            tEnv' = bindVars bs' tEnv
            output = unlines [x ++ " : " ++ show t | (x,t) <- bs']
        in put (vEnv', tEnv') >> lift (putStr output)
evalStmt (LetRecStmt bs) = do
  (vEnv,tEnv) <- get
  case runInferRecBinds tEnv bs of
    Left err -> lift (print err)
    Right bs' ->
      let vEnv' = bindVars [(f, VClosure x e vEnv') | (f,x,e) <- bs] vEnv
          output = unlines [f ++ " : " ++ show t | (f,t) <- bs']
      in put (vEnv', bindVars bs' tEnv) >> lift (putStr output)

readEvalPrint :: String -> REPL ()
readEvalPrint str = case parseStmt str of
  Left err  -> lift (print err)
  Right stm -> evalStmt stm

readEvalFile :: FilePath -> REPL ()
readEvalFile fp = do
  str <- lift (readFile fp)
  case parseFile str of
    Left err   -> lift (print err)
    Right stms -> mapM_ evalStmt stms

loop :: REPL ()
loop = do
  input <- lift (readPrompt ">> ")
  case parseCommand input of
    Right (ComEvalStmt stm) -> evalStmt stm >> loop
    Right (ComEvalFile fp)  -> readEvalFile fp >> loop
    Right Quit              -> return ()
    Left err                -> lift (print err) >> loop

runREPL :: IO ()
runREPL = evalStateT loop mempty
