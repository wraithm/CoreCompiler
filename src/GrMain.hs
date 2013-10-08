module TiMain where

import qualified System.Environment as S

import GRMachine
import Parser
import LambdaLift

-- testProg = "main = let x = if false 3 5; y = 2 in x; true = \\x y. x; false = \\x y. y; if = \\p x y.  p x y;"
-- prog = parseProgram testProg
-- main = (putStrLn . show . runProgram . lambdaLift) prog

main :: IO ()
main = do
    args <- S.getArgs
    if length args > 0 then do
        let file:_ = args
        prog <- parseProgramFromFile file
        (putStrLn . show . runProgram . lambdaLift) prog
    else do
        putStrLn "Need a file to parse."
        return ()