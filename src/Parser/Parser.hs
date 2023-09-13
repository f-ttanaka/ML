module Parser.Parser where

import           Data.Foldable      (Foldable (foldr'))
import           Error
import           Parser.Lexer
import           REPL.Prompt
import           Syntax
import           Text.Parsec
import           Text.Parsec.String (Parser)

intLit :: Parser Expr
intLit = LInt . fromIntegral <$> natural

boolLit :: Parser Expr
boolLit = LBool
  <$> (True <$ reserved "true"
      <|> False <$ reserved "false")

varExpr :: Parser Expr
varExpr = Var <$> identifier

ifExpr :: Parser Expr
ifExpr = IfExpr
  <$> (reserved "if" >> expr)
  <*> (reservedOp "then" >> expr)
  <*> (reserved "else" >> expr)

letBind :: Parser [(Name, Expr)]
letBind = reserved "let" >> sepBy1 bind (reserved "and")
  where
    bind :: Parser (Name, Expr)
    bind = do
      f <- identifier
      xs <- many identifier
      reservedOp "="
      e <- expr
      return (f, foldr' Lambda e xs)

letRecBind :: Parser [(Name, Name, Expr)]
letRecBind = reserved "let" >> reserved "rec" >> sepBy1 bind (reserved "and")
  where
    bind :: Parser (Name, Name, Expr)
    bind = do
      f <- identifier
      x <- identifier
      xs <- many identifier
      reservedOp "="
      e <- expr
      return (f, x, foldr' Lambda e xs)

lambda :: Parser Expr
lambda = do
  reserved "fun"
  x <- identifier
  xs <- many identifier
  reservedOp "->"
  bod <- expr
  return (foldr Lambda (Lambda x bod) xs)

aexpr :: Parser Expr
aexpr = parens expr
  <|> intLit
  <|> boolLit
  <|> ifExpr
  <|> try (LetRecExpr <$> letRecBind <* reserved "in" <*> expr)
  <|> LetExpr <$> letBind <* reserved "in" <*> expr
  <|> lambda
  <|> varExpr

term :: Parser Expr
term = aexpr >>= \x ->
  (many1 aexpr >>= \xs -> return (foldl App x xs))
  <|> return x


table :: Operators Expr
table = [
    [
      infixOpAL "*" (BinExpr Mul)
    ],
    [
      infixOpAL "+" (BinExpr Add),
      infixOpAL "-" (BinExpr Sub)
    ],
    [
      infixOpAL "==" (BinExpr Eq),
      infixOpAL "<" (BinExpr Lt)
    ]
  ]

expr :: Parser Expr
expr = contents table term

checkParse :: Parser a -> String -> String -> Check a
checkParse p pName str = case parse p pName str of
  Left err  -> throw (Parse err)
  Right res -> return res

parseExpr :: String -> Check Expr
parseExpr str = checkParse expr "expr" str

endWithSemi :: Parser a -> Parser a
endWithSemi p = do
  x <- p
  char ';'>> return x

exprStm :: Parser Stmt
exprStm = ExprStmt <$> expr

stmt :: Parser Stmt
stmt = try (ExprStmt <$> expr <* symbol ";")
  <|> try (LetRecStmt <$> letRecBind <* symbol ";")
  <|> LetStmt <$> letBind <* symbol ";"

parseStmt :: String -> Check Stmt
parseStmt str = checkParse stmt "statement" str

-- REPL Commands

command :: Parser Command
command = try (ComEvalStmt <$> stmt)
  <|> ComEvalFile <$> (symbol ":l" >> manyTill anyChar space)
  <|> (string "quit" >> return Quit)

parseCommand :: String -> Check Command
parseCommand str = checkParse command "command" str

file :: Parser [Stmt]
file = many stmt

parseFile :: String -> Check [Stmt]
parseFile str = checkParse file "file" str
