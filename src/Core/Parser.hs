module Core.Parser where

import Control.Applicative ((<$>))

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Expr as E

import Core.Lexer
import Core.AST

parseProgram :: String -> CoreProgram
parseProgram t = case parse pProg "" t of
    Left err -> error $ show err
    Right defs -> defs

parseProgramFromFile :: FilePath -> IO CoreProgram
parseProgramFromFile fp = do
    result <- parseFromFile pProg fp 
    case result of
        Left err -> do
            print err
            error $ "Failed to parse " ++ show fp
        Right defs -> return defs

pProg :: Parser CoreProgram
pProg = allOf (many1 pDefn)

pDefn :: Parser CoreDefn
pDefn = do
     name <- identifier
     args <- many identifier
     equals
     e <- pExpr
     semi
     return (name, args, e)

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
    xs <- many identifier
    arrow
    e <- pExpr
    return $ (i, xs, e)
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
    <?> "let"

pLetRec :: Parser CoreExpr
pLetRec = do
    reserved "letrec"
    defs <- pDef `sepBy1` semi
    reserved "in"
    ein <- pExpr
    return $ Let True defs ein -- Maybe add itself to defs at parsing level?
    <?> "letrec"

pFactor :: Parser CoreExpr
pFactor = parens pExpr
    <|> pVar
    <|> pNum
    <|> pLambda
    <|> pLet
    <|> pLetRec
    <|> pCase
    <?> "factor"

pTerm :: Parser CoreExpr
pTerm = E.buildExpressionParser table pFactor <?> "term"
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
    <?> "expr"

parseExpr :: String -> CoreExpr
parseExpr t = case parse (allOf pExpr) "" t of
    Left err -> error $ show err
    Right ast -> ast
