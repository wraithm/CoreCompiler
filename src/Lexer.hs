module Lexer where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

coreDef = emptyDef
    { T.commentStart    = "{-"
    , T.commentEnd      = "-}"
    , T.commentLine     = "--"
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

lexer :: T.TokenParser ()
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
lexeme      = T.lexeme lexer

lambda = reservedOp "\\"
equals = reservedOp "="
arrow  = reservedOp "->"

allOf p = do
    whitespace
    r <- p
    eof
    return r
