
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           System.IO (readFile)

import qualified Data.Map as Map

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String
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
  = AssocN
  | AssocL
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

scn :: Parser ()
scn = L.space (void spaceChar) empty empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

operatorChar :: Parser Char
operatorChar = symbolChar <|> punctuationChar <|> char '-'

operator :: Parser String
operator = some operatorChar <?> "operator"

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

termParser :: Parser Expr -> Parser Expr
termParser expr = Lit <$> integer <|> parens expr

fixityDeclsParser :: Parser [FixityDecl]
fixityDeclsParser = sepEndBy fixityDeclParser scn

fixityDeclParser :: Parser FixityDecl
fixityDeclParser = label "fixity declaration" $ do
  fix  <- fixity
  prec <- precedence
  op   <- operator
  return $ FixityDecl op fix prec
    where
      precedence = Prec <$> option 9 integer

      fixity =  reserved "prefix"  *> pure FixPre
            <|> reserved "postfix" *> pure FixPost
            <|> reserved "infixl"  *> pure (FixInf AssocL)
            <|> reserved "infixr"  *> pure (FixInf AssocR)
            <|> reserved "infix"   *> pure (FixInf AssocN)

fixityDeclToOperator :: FixityDecl -> Operator Parser Expr
fixityDeclToOperator (FixityDecl name fixity _) = makeOp fixity
  where
    makeOp FixPre         = prefix       name (UnaryOp name)
    makeOp FixPost        = postfix      name (UnaryOp name)
    makeOp (FixInf assoc) = binary assoc name (BinaryOp name)

binary :: Assoc -> String -> (a -> a -> a) -> Operator Parser a
binary AssocN name f = InfixN  (f <$ symbol name)
binary AssocL name f = InfixL  (f <$ symbol name)
binary AssocR name f = InfixR  (f <$ symbol name)

prefix, postfix :: String -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

groupSortFixityDeclsByPrec :: [FixityDecl] -> [[FixityDecl]]
groupSortFixityDeclsByPrec decls = snd <$> reverse (Map.toList groupedByPrec)
  where
    groupedByPrec = Map.fromListWith (++) precDecls
    precDecls     = [(prec, [decl]) | decl@(FixityDecl _ _ prec) <- decls]

buildOperatorTable :: [FixityDecl] -> [[Operator Parser Expr]]
buildOperatorTable decls = fmap fixityDeclToOperator <$> sortedDecls
  where
    sortedDecls = preDecls ++ postDecls ++ infDecls
    preDecls    = groupSortFixityDeclsByPrec [decl | decl@(FixityDecl _ FixPre  _)    <- decls]
    postDecls   = groupSortFixityDeclsByPrec [decl | decl@(FixityDecl _ FixPost _)    <- decls]
    infDecls    = groupSortFixityDeclsByPrec [decl | decl@(FixityDecl _ (FixInf _) _) <- decls]

buildExprParser :: Parser Expr -> [FixityDecl] -> Parser Expr
buildExprParser p = makeExprParser p . buildOperatorTable

parseTopLevel :: Parser a -> Parser a
parseTopLevel = between scn (scn >> eof)

topLevelParser :: Parser Expr
topLevelParser = parseTopLevel $ do
  fixityDecls <- fixityDeclsParser
  exprParser fixityDecls

exprParser :: [FixityDecl] -> Parser Expr
exprParser decls = exprParser'
  where exprParser' = buildExprParser (termParser exprParser') decls

parseFile :: String -> IO Expr
parseFile file = do
  contents <- readFile file
  case parse topLevelParser file contents of
    Left err  -> error (parseErrorPretty err)
    Right res -> return res

