
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
  = Lit SourcePos Integer
  | UnaryOp SourcePos Name Expr
  | BinaryOp SourcePos Name Expr Expr
  deriving (Eq, Show)

pprint :: Expr -> String
pprint (Lit _ i)          = show i
pprint (UnaryOp _ n e)    = concat ["(", n, " ", pprint e, ")"]
pprint (BinaryOp _ n l r) = concat ["(", pprint l, " ", n, " ", pprint r, ")"]

setPos :: SourcePos -> Expr -> Expr
setPos pos (Lit _ n)          = Lit pos n
setPos pos (UnaryOp _ n e)    = UnaryOp pos n e
setPos pos (BinaryOp _ n l r) = BinaryOp pos n l r

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

withPos :: (SourcePos -> Parser a) -> Parser a
withPos f = getPosition >>= f

termParser :: Parser Expr -> Parser Expr
termParser expr = withPos $ \pos ->
  Lit pos <$> integer <|> parens expr

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
    makeOp FixPre         = prefix       name (flip UnaryOp  name)
    makeOp FixPost        = postfix      name (flip UnaryOp  name)
    makeOp (FixInf assoc) = binary assoc name (flip BinaryOp name)

binary :: Assoc -> String -> (SourcePos -> a -> a -> a) -> Operator Parser a
binary AssocN name f = InfixN (mkPosOp name f)
binary AssocL name f = InfixL (mkPosOp name f)
binary AssocR name f = InfixR (mkPosOp name f)

prefix, postfix :: String -> (SourcePos -> a -> a) -> Operator Parser a
prefix  name f = Prefix  (mkPosOp name f)
postfix name f = Postfix (mkPosOp name f)

mkPosOp :: String -> (SourcePos -> a) -> Parser a
mkPosOp name f = withPos (\pos -> f pos <$ symbol name)

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

