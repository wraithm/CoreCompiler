module Parser where

import Control.Applicative ((<$>))

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E

import Lexer
import AST

-- TODO Add toplevel combinator definitions

pInt :: Parser Int
pInt = fromInteger <$> integer <?> "Int"

pVar :: Parser CoreExpr
pVar = Var <$> identifier <?> "Var"

pNum :: Parser CoreExpr
pNum = Num <$> pInt <?> "Num"

pConst :: Parser CoreExpr
pConst = do
    reserved "Pack"
    braces $ do
        i <- pInt
        comma
        j <- pInt
        return $ Constr i j
    <?> "Constructor"

pCase :: Parser CoreExpr
pCase = do
    reserved "case"
    e <- pExpr
    reserved "of"
    alts <- pAlter `sepBy1` semi
    return $ Case e alts
    <?> "Case"

pAlter :: Parser CoreAlt
pAlter = do
    i <- angles pInt
    -- Gotta get variables in here
    arrow
    e <- pExpr
    return $ (i, [], e)
    <?> "Case"

pLambda :: Parser CoreExpr
pLambda = do
    lambda
    xs <- many1 identifier -- Convert to many idents
    dot
    e <- pExpr
    return $ Lam xs e
    <?> "Lambda"

pDef :: Parser (Name, CoreExpr)
pDef = do
    x <- identifier
    equals
    e <- pExpr
    return (x, e)
    <?> "Definition"

pLet :: Parser CoreExpr
pLet = do
    reserved "let"
    defs <- pDef `sepBy1` semi
    reserved "in"
    ein <- pExpr
    return $ Let False defs ein

pLetRec :: Parser CoreExpr
pLetRec = do
    reserved "letrec"
    defs <- pDef `sepBy1` semi
    reserved "in"
    ein <- pExpr
    return $ Let True defs ein -- Maybe add itself to defs at parsing level?

pFactor :: Parser CoreExpr
pFactor = parens pExpr
    <|> pVar
    <|> pNum
    <|> pLambda
    <|> pLet
    <|> pLetRec
    <|> pCase

pTerm :: Parser CoreExpr
pTerm = E.buildExpressionParser table pFactor
  where infixOp x = E.Infix (reservedOp x >> return (\e1 e2 -> App (App (Var x) e1) e2))
        table = 
            [ [ infixOp "*" E.AssocLeft 
              ]
            , [ infixOp "+" E.AssocLeft
              , infixOp "-" E.AssocLeft
              ]
            ]

pExpr :: Parser CoreExpr
pExpr = foldl1 App <$> many1 pTerm

parseExpr :: String -> CoreExpr
parseExpr t = case parse (allOf pExpr) "" t of
    Left err -> error $ show err
    Right ast -> ast
