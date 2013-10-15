module GrMain where

import qualified System.Environment as S

import GraphReduction.Compiler (runProgram)
import Core.LambdaLift (lambdaLift)
import Core.Parser (parseProgramFromFile)
import Core.Pretty (prettyProgram)

-- testProg = "main = let x = if false 3 5; y = 2 in x; true = \\x y. x; false = \\x y. y; if = \\p x y.  p x y;"
-- prog = parseProgram testProg
-- main = (putStrLn . show . runProgram . lambdaLift) prog

main :: IO ()
main = do
    args <- S.getArgs
    if not (null args) then do
        let file:_ = args
        prog <- parseProgramFromFile file
        (putStrLn . prettyProgram) prog
        breakln
        (putStrLn . prettyProgram . lambdaLift) prog
        breakln
        (print . runProgram . lambdaLift) prog
    else do
        putStrLn "Need a file to parse."
        return ()
  where
    breakln = putStrLn "---------------------------"
