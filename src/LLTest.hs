module LLTest where

import GraphReduction.Compiler (runProgram)

import Core.LambdaLift (lambdaLift, freeVars, abstract, rename)
import Core.Parser (parseProgram)
import Core.Pretty (prettyProgram)

-- prog = "true x y = x; if p x y = p x y; main = letrec f = \\x. if true 3 (f 3) in f 3;"
-- prog = "main = let true = \\x y. x; false = \\x y. y; if = \\p x y. p x y in if true 3 4;"
prog = "test = (case x of <1> -> \\x.x; <2> -> \\x y. x); main = test;"

main = do
    let parsed = parseProgram prog
        lifted = lambdaLift parsed
    printPretty parsed
    break
    printPretty (step1 parsed)
    break
    printPretty (step2 parsed)
    break
    printPretty lifted
  where
    break = putStrLn "--------------------------"
    printPretty = putStrLn . prettyProgram
    step1 = abstract . freeVars
    step2 = rename . step1
