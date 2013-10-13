module Core.Prelude where

import Core.AST

preludeDefs :: CoreProgram
preludeDefs =
    [ ("I", ["x"], Var "x") -- Define SKI combinators for Core
    , ("K", ["x","y"], Var "x")
    , ("K1", ["x","y"], Var "y")
    , ("S", ["f", "g", "x"],
        App (App (Var "f") (Var "x"))
            (App (Var "g") (Var "x")))
    , ("compose", ["f","g","x"],
        App (Var "f") (App (Var "g") (Var "x")))
    , ("twice", ["f"], App (App (Var "compose") (Var "f")) (Var "f")) 
    ]

