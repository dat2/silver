{-# LANGUAGE OverloadedStrings #-}

module Main where

import JIT

import qualified Data.Text.Lazy.IO as T
import LLVM.AST (Module)
import LLVM.AST.Name (Name(..))
import LLVM.AST.Type (i32)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Instruction (ret)
import LLVM.IRBuilder.Module (buildModule, function)
import LLVM.Pretty (ppllvm)

testModule :: Module
testModule = buildModule "anonymous" builder
  where
    builder = do
        function
            (Name "main")
            []
            i32
            (\_ -> do
                 zero <- int32 0
                 ret zero)

main :: IO ()
main = do
    T.putStrLn (ppllvm testModule)
    r <- jit testModule
    print r
