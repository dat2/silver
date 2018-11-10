module Main where

import qualified Data.Text.Lazy.IO as T
import JIT
import LLVM.Pretty (ppllvm)
import STG
import STGCodeGen

testProg =
    Prog [Binding "main" (LambdaForm [] NonUpdateable [] (Lit (LInt32 0)))]

main :: IO ()
main = do
    let testModule = codegen testProg
    T.putStrLn (ppllvm testModule)
    r <- jit testModule
    print r
