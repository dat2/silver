module Main where

import Core

exampleProgram :: Program
exampleProgram =
    [ NonRecursive
          (Id "main")
          (TyArrow TyInt32 TyInt32)
          (Lam (Id "x") (TyInt32) (Var (Id "x")))
    , NonRecursive
          (Id "other")
          (TyInt32)
          (Let (NonRecursive (Id "x") TyInt32 (Lit (LInt32 5))) (Var (Id "x")))
    ]

main :: IO ()
main = print exampleProgram
