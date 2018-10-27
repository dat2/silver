{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core
import Id
import Types

exampleProgram :: Program
exampleProgram =
    [ NonRec
          (Variable "main" (tyInt32 `tyFn` tyInt32))
          (Lam (Variable "x" tyInt32) (Var "x"))
    , NonRec
          (Variable "other" (tyInt32 `tyFn` tyInt32))
          (Let (NonRec (Variable "x" tyInt32) (Lit (LInt 5))) (Var "x"))
    ]

main :: IO ()
main = print exampleProgram
