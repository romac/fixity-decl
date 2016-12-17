
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import           Control.Monad (void)
import           System.IO (readFile)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L

type Name = String

data Expr
  = Neg Expr
  | Lit Integer
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  deriving (Eq, Show)

pprint :: Expr -> String
pprint (Neg e)          = concat ["(-", pprint e, ")"]
pprint (Lit i)          = show i
pprint (UnaryOp n e)    = concat ["(", n, " ", pprint e, ")"]
pprint (BinaryOp n l r) = concat ["(", pprint l, " ", n, " ", pprint r, ")"]

data Assoc
  = AssocL
  | AssocR
  deriving (Eq, Show)

data Fixity
  = FixPre
  | FixPost
  | FixInf Assoc
  deriving (Eq, Show)

newtype Prec
  = Prec Integer
  deriving (Eq, Ord, Num, Show)

data FixityDecl
  = FixityDecl Name Fixity Prec
  deriving (Eq, Show)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

operators :: Set Char
operators = Set.fromList ['-', ':']

isOperator c = Set.member c operators

operator :: Parser Char
operator = (try symbolChar <|> satisfy isOperator) <?> "operator"

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

semi :: Parser ()
semi = void $ symbol ";"

termParser :: Parser Expr -> Parser Expr
termParser exprParser
   =  try (Lit <$> integer)
  <|> try (parens exprParser)

fixityDeclsParser :: Parser [FixityDecl]
fixityDeclsParser = sepEndBy1 fixityDeclParser semi

fixityDeclParser :: Parser FixityDecl
fixityDeclParser = prefix <|> postfix <|> infixL <|> infixR
  where
    prefix = do
      reserved "prefix"
      name <- parens (some operator)
      return $ FixityDecl name FixPre (Prec 999999)

    postfix = do
      reserved "postfix"
      name <- parens (some operator)
      return $ FixityDecl name FixPost (Prec 888888)

    infixL = do
      reserved "infixl"
      name <- parens (some operator)
      prec <- integer
      return $ FixityDecl name (FixInf AssocL) (Prec prec)

    infixR = do
      reserved "infixr"
      name <- parens (some operator)
      prec <- integer
      return $ FixityDecl name (FixInf AssocR) (Prec prec)

groupSortFixityDeclsByPrec :: [FixityDecl] -> [[FixityDecl]]
groupSortFixityDeclsByPrec decls = snd <$> reverse (Map.toList grouped)
  where
    grouped   = Map.fromListWith (++) precDecls
    precDecls = [(prec, [decl]) | decl@(FixityDecl _ _ prec) <- decls]

fixityDeclToOperator :: FixityDecl -> Operator Parser Expr
fixityDeclToOperator (FixityDecl name (FixInf assoc) _) = binary  assoc name (BinaryOp name)
fixityDeclToOperator (FixityDecl name FixPre _)         = prefix        name (UnaryOp name)
fixityDeclToOperator (FixityDecl name FixPost _)        = postfix       name (UnaryOp name)

binary AssocL name f = InfixL  (f <$ symbol name)
binary AssocR name f = InfixR  (f <$ symbol name)
prefix        name f = Prefix  (f <$ symbol name)
postfix       name f = Postfix (f <$ symbol name)

defaultPrefixOps = [ [ prefix "-" Neg ] ]

buildOperatorTable :: [FixityDecl] -> [[Operator Parser Expr]]
buildOperatorTable decls = defaultPrefixOps ++ infixOps
  where
    infixOps    = fmap fixityDeclToOperator <$> sortedDecls
    sortedDecls = groupSortFixityDeclsByPrec decls

buildExprParser :: Parser Expr -> [FixityDecl] -> Parser Expr
buildExprParser p = makeExprParser p . buildOperatorTable

topLevelParser :: Parser Expr
topLevelParser = do
  sc
  fixityDecls <- fixityDeclsParser
  let exprParser = exprParser' fixityDecls
  expr <- exprParser
  eof
  return expr

exprParser' :: [FixityDecl] -> Parser Expr
exprParser' decls =
  let exprParser = buildExprParser (termParser exprParser) decls
   in exprParser

parseFile :: String -> IO Expr
parseFile file = do
  contents <- readFile file
  case parse topLevelParser file contents of
    Left err  -> error (parseErrorPretty err)
    Right res -> return res

