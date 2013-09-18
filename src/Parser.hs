module Parser where

import Control.Applicative ((<$>),(<*>))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

import AST

coreDef = emptyDef
    { T.commentStart    = "{-"
    , T.commentEnd      = "-}"
    , T.nestedComments  = True
    , T.identStart      = letter
    , T.identLetter     = alphaNum <|> oneOf "_'"
    , T.opStart         = T.opLetter coreDef
    , T.opLetter        = oneOf "=+/*-~><&|\\"
    , T.reservedNames   = 
        [ "case"
        , "of"
        , "let"
        , "in"
        , "letrec"
        , "Pack"
        ]
    }

lexer = T.makeTokenParser coreDef

parens      = T.parens lexer
braces      = T.braces lexer
brackets    = T.brackets lexer
angles      = T.angles lexer
semi        = T.semi lexer
comma       = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
integer     = T.integer lexer
strLiteral  = T.stringLiteral lexer
operator    = T.operator lexer
reservedOp  = T.reservedOp lexer
whitespace  = T.whiteSpace lexer
dot         = T.dot lexer

lambda = reservedOp "\\"
equals = reservedOp "="
arrow = reservedOp "->"

pInt = fromInteger <$> integer

--parseVar :: Parser Expr
parseVar = Var <$> identifier

parseInt = Num <$> pInt

--parseOp :: Parser Expr
parseOp = do
    e1 <- parseExpr
    opName <- operator
    e2 <- parseExpr
    return $ App (App (Var opName) e1) e2

--parseConst :: Parser Expr
parseConst = do
    reserved "Pack"
    braces $ do
        i <- pInt
        comma
        j <- pInt
        return $ Constr i j

parseApp :: Parser CoreExpr
parseApp = chainl1 parseExpr (return App)

parseCase = do
    reserved "case"
    e <- parseExpr
    reserved "of"
    alts <- parseAlter `sepBy1` semi
    return $ Case e alts

parseAlter :: Parser (CoreAlt)
parseAlter = do
    i <- angles pInt
    arrow
    e <- parseExpr
    return $ (i, [], e)

parseExpr :: Parser (CoreExpr)
parseExpr = try (parens parseExpr)
    <|> try parseApp
    <|> try parseVar
    <|> try parseInt
    <|> try parseOp
    <|> try parseConst
