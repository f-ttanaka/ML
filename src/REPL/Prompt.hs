module REPL.Prompt where

import           Syntax    (Stmt)
import           System.IO

data Command =
  ComEvalStmt Stmt
  | ComEvalFile FilePath
  | Quit


flushStr :: String -> IO ()
flushStr str = do
  putStr str
  hFlush stdout

readPrompt :: String -> IO String
readPrompt pr = do
  flushStr pr
  getLine
