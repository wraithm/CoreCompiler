module GrMain where

import qualified System.Environment as S
import Data.Map (toList)
import Control.Monad (forM_)

import GraphReduction.Compiler (runProgram, compile)
import GraphReduction.Machine (eval, code, dump, stack, heap)
import Util.Heap (objects)
import Core.LambdaLift (lambdaLift)
import Core.Parser (parseProgramFromFile)
import Core.Pretty (prettyProgram)

-- testProg = "main = let x = if false 3 5; y = 2 in x; true = \\x y. x; false = \\x y. y; if = \\p x y.  p x y;"
-- prog = parseProgram testProg
-- main = (putStrLn . show . runProgram . lambdaLift) prog

breakln = putStrLn "---------------------------"

printEval prog = do
    forM_ (eval $ compile prog) $ \s -> do
        breakln
        putStr "Code: "
        print $ code s
        putStr "Dump: "
        print $ dump s
        putStr "Stack: "
        print $ stack s
        putStrLn "Heap Objs: "
        putStrLn . unlines . map (\(x,y) -> show x ++ "\t\t" ++ show y) . toList . objects . heap $ s

main :: IO ()
main = do
    args <- S.getArgs
    if not (null args) then do
        let file:_ = args

        prog <- parseProgramFromFile file
        let prettyprog = prettyProgram prog
            lambdaprog = lambdaLift prog

        putStrLn prettyprog
        breakln
        putStrLn $ prettyProgram lambdaprog
        breakln
        printEval lambdaprog
    else do
        putStrLn "Need a file to parse."
        return ()
