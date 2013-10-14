module Core.Pretty where

import Text.PrettyPrint

import Core.AST
import Core.Parser

prettyProgram :: CoreProgram -> String
prettyProgram = render . semiSep . map prettyDefn

semiSep :: [Doc] -> Doc
semiSep = vcat . punctuate semi

spaceSep :: [String] -> Doc
spaceSep = sep . map text

prettyDefn :: CoreDefn -> Doc
prettyDefn (name, args, e) = 
    text name <+> spaceSep args <+> equals <+> prettyExp e

prettyExp :: CoreExpr -> Doc
prettyExp (Var n) = text n
prettyExp (Num n) = int n
prettyExp (Constr i j) = text "Pack" <> braces (int i <> text "," <> int j)
prettyExp (App e1 e2) = prettyExp e1 <+> prettyExp e2
prettyExp (Let isRec defs e) = 
    text (if isRec then "letrec" else "let") <+> 
    nest 1 (prettyDefs defs) $$
    text "in" <+> prettyExp e
prettyExp (Case e alts) = 
    text "case" <+> prettyExp e <+> text "of" <> nest 1 (prettyAlts alts)
prettyExp (Lam args e) = text "\\" <> spaceSep args <> dot <+> prettyExp e
  where dot = text "."

prettyDefs :: [(Name, CoreExpr)] -> Doc
prettyDefs = semiSep . map prettyDef

prettyDef :: (Name, CoreExpr) -> Doc
prettyDef (name, e) = text name <+> equals <+> prettyExp e

prettyAlts :: [CoreAlt] -> Doc
prettyAlts = semiSep . map prettyAlt

prettyAlt :: CoreAlt -> Doc
prettyAlt (i, args, e) = carots (int i) <+> spaceSep args <+> text "->" <+> prettyExp e
  where carots x = text "<" <> x <> text ">"
