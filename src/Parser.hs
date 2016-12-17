
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
  = BinOp Name Expr Expr
  | Lit Integer
  | Neg Expr
  deriving (Eq, Show)

pprint :: Expr -> String
pprint (BinOp n l r) = concat ["(", pprint l, " ", n, " ", pprint r, ")"]
pprint (Lit i)       = show i
pprint (Neg e)       = "(-" ++ pprint e ++ ")"

data Assoc
  = AssocL
  | AssocR
  deriving (Eq, Show)

newtype Prec
  = Prec Integer
  deriving (Eq, Ord, Num, Show)

data FixityDecl
  = FixityDecl Name Assoc Prec
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
fixityDeclParser = try leftInfix <|> rightInfix
  where
    leftInfix = do
      reserved "infixl"
      name <- parens (some operator)
      prec <- integer
      return $ FixityDecl name AssocL (Prec prec)

    rightInfix = do
      reserved "infixr"
      name <- parens (some operator)
      prec <- integer
      return $ FixityDecl name AssocR (Prec prec)

groupSortFixityDeclsByPrec :: [FixityDecl] -> [[FixityDecl]]
groupSortFixityDeclsByPrec decls = snd <$> reverse (Map.toList grouped)
  where
    grouped   = Map.fromListWith (++) precDecls
    precDecls = [(prec, [decl]) | decl@(FixityDecl _ _ prec) <- decls]

fixityDeclToOperator :: FixityDecl -> Operator Parser Expr
fixityDeclToOperator (FixityDecl name assoc _) = makeOp (assocToInfix assoc) name (BinOp name)
  where
    assocToInfix AssocL = InfixL
    assocToInfix AssocR = InfixR

makeOp assoc name f = assoc (f <$ symbol name)
prefix = makeOp Prefix

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

