{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core
import Name
import Types

exampleProgram :: Program
exampleProgram =
    [ NonRec (Id "main" (int32 `fn` int32)) (Lam (Id "x" int32) (Var "x"))
    , NonRec
          (Id "other" (int32 `fn` int32))
          (Let (NonRec (Id "x" int32) (Lit (LitInt 5))) (Var "x"))
    ]

main :: IO ()
main = print exampleProgram
