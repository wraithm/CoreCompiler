module TiMain where

import TiState
import Parser

testProg = "main = if false 3 5; true x y = x; false x y = y; if p x y = p x y;"
prog = parseProgram testProg

main = (putStrLn . show . runProgram) prog
