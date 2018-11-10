{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module JIT
    ( jit
    ) where

import Data.Int (Int32)
import Foreign.Ptr (FunPtr, castPtrToFunPtr, wordPtrToPtr)
import qualified LLVM.AST as AST
import LLVM.Analysis (verify)
import LLVM.Context (withContext)
import LLVM.Internal.OrcJIT (JITSymbol(..), JITSymbolError(..))
import LLVM.Internal.OrcJIT.CompileLayer (findSymbol)
import LLVM.Module (Module, withModuleFromAST)
import LLVM.OrcJIT
       (JITSymbol, JITSymbolError, MangledSymbol, SymbolResolver(..),
        mangleSymbol, withIRCompileLayer, withModule,
        withObjectLinkingLayer)
import LLVM.Target (withHostTargetMachine)

foreign import ccall "dynamic" mkMain ::
               FunPtr (IO Int32) -> IO Int32

nullResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
nullResolver _ = return $ Left $ JITSymbolError "Not implemented."

jit :: AST.Module -> IO Int32
jit astModule =
    withContext $ \context ->
        withModuleFromAST context astModule $ \m -> do
            verify m
            executeModule m

executeModule :: Module -> IO Int32
executeModule m =
    withHostTargetMachine $ \targetMachine ->
        withObjectLinkingLayer $ \linkingLayer ->
            withIRCompileLayer linkingLayer targetMachine $ \irCompileLayer ->
                withModule
                    irCompileLayer
                    m
                    (SymbolResolver nullResolver nullResolver) $ \mHandle -> do
                    mainSymbol <- mangleSymbol irCompileLayer "main"
                    Right (JITSymbol {jitSymbolAddress}) <-
                        findSymbol irCompileLayer mainSymbol True
                    result <-
                        mkMain (castPtrToFunPtr (wordPtrToPtr jitSymbolAddress))
                    return result
