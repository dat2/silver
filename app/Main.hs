module Main where

import Core

exampleProgram :: Program
exampleProgram =
    [ Bind
          (Id "main")
          (TyArrow TyInt32 TyInt32)
          (Lam (Id "x") (TyInt32) (Var (Id "x")))
    ]

main :: IO ()
main = print exampleProgram
