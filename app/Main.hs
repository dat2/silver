module Main where

import Core

exampleProgram :: Program
exampleProgram =
    [ letn "main" (tyInt32 :-> tyInt32) (Lam (Id "x") (tyInt32) (Var (Id "x")))
    , letn
          "other"
          (tyInt32)
          (Let (NonRecursive (Id "x") tyInt32 (Lit (LInt 5))) (Var (Id "x")))
    ]

main :: IO ()
main = print exampleProgram
