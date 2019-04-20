module Compiler where

import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text.Prettyprint.Doc

newtype Ast =
    Program [Expression]
    deriving (Show, Eq)

data Expression =
    NumberLiteral Int
  | StringLiteral String
  | CallExpression String [Expression]
    deriving (Show, Eq)

newtype NewAst =
    NewProgram [NewExpressionStatement]
    deriving (Show, Eq)

newtype NewExpressionStatement = NewExpressionStatement NewExpression
    deriving (Show, Eq)

data NewExpression =
    NewNumberLiteral Int
  | NewStringLiteral String
  | NewCallExpression NewIdentifier [NewExpression]
    deriving (Show, Eq)

newtype NewIdentifier = NewIdentifier String
    deriving (Show, Eq)

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

pProgram :: Parser Ast
pProgram = Program <$> (spaceConsumer *> many pExpression)

pExpression :: Parser Expression
pExpression = NumberLiteral <$> pInt
              <|> StringLiteral <$> pStringLiteral
              <|> pCallExpression

pCallExpression :: Parser Expression
pCallExpression =
    uncurry CallExpression <$> pParens ((,) <$> pName <*> many pExpression)

pInt :: Parser Int
pInt = lexeme L.decimal

pStringLiteral :: Parser String
pStringLiteral = lexeme (C.char '"' *> manyTill C.asciiChar (C.char '"'))

pName :: Parser String
pName = lexeme (some C.lowerChar)

pSymbol :: String -> Parser String
pSymbol = L.symbol spaceConsumer

pParens :: Parser a -> Parser a
pParens = between (pSymbol "(") (pSymbol ")")

parser :: String -> Either (ParseErrorBundle String Void) Ast
parser = parse (pProgram <* eof) ""

transformer :: Ast -> NewAst
transformer (Program topLevelExpressions) =
    NewProgram (NewExpressionStatement . transformExpression
                <$> topLevelExpressions)

transformExpression :: Expression -> NewExpression
transformExpression (NumberLiteral value) = NewNumberLiteral value
transformExpression (StringLiteral value) = NewStringLiteral value
transformExpression (CallExpression name params) =
    NewCallExpression (NewIdentifier name) (transformExpression <$> params)

codeGenerator :: NewAst -> String
codeGenerator = show . prettyNewAst

prettyNewAst :: NewAst -> Doc ann
prettyNewAst (NewProgram statements) = vsep (prettyStatement <$> statements)

prettyStatement :: NewExpressionStatement -> Doc ann
prettyStatement (NewExpressionStatement e) = prettyExpression e <> pretty ';'

prettyExpression :: NewExpression -> Doc ann
prettyExpression (NewNumberLiteral value) =
    pretty value
prettyExpression (NewStringLiteral value) =
    pretty '"' <> pretty value <> pretty '"'
prettyExpression (NewCallExpression (NewIdentifier name) arguments) =
    pretty name
    <> encloseSep lparen rparen (pretty ", ") (prettyExpression <$> arguments)

compiler :: String -> Either (ParseErrorBundle String Void) String
compiler = (codeGenerator . transformer <$>) . parser
