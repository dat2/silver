module Main where

import Core

exampleProgram :: Program
exampleProgram =
    [ letn "main" (Lam (Id "x") (tyInt32) (Var (Id "x")))
    , letn "other" (Let (NonRecursive (Id "x") (Lit (LInt 5))) (Var (Id "x")))
    ]

main :: IO ()
main = print exampleProgram
