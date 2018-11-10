{-# LANGUAGE OverloadedStrings #-}

module STGCodeGen where

import STG

import Control.Monad (mapM)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import LLVM.AST (Module)
import LLVM.AST.Constant (Constant(..))
import LLVM.AST.Name (Name(..))
import LLVM.AST.Operand (Operand(..))
import LLVM.AST.Type (Type(..), i32, ptr, void)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Instruction
       (call, extractValue, insertValue, ret, retVoid)
import LLVM.IRBuilder.Module
       (MonadModuleBuilder, buildModule, function)
import LLVM.IRBuilder.Monad (MonadIRBuilder)

codegen :: Prog -> Module
codegen (Prog bindings) =
    buildModule "anonymous" $ do
        globalEntrys <- mapM codegenGlobalEntrys bindings
        let globalEntrysMap =
                Map.fromList $ zip (map bName bindings) globalEntrys
        codegenMain globalEntrysMap

entryFunctionPointerType :: Type
entryFunctionPointerType = ptr (FunctionType void [] False)

closureType :: Type
closureType = StructureType True [entryFunctionPointerType]

codegenMain :: MonadModuleBuilder m => Map.Map String Operand -> m ()
codegenMain globalEntrys =
    function (Name "main") [] i32 mainBodyBuilder >> return ()
  where
    mainBodyBuilder _ = do
        initialHeap <- mapM insertClosure $ Map.toList globalEntrys
        let globalEnvironment = Map.fromList initialHeap
        enter (fromJust $ Map.lookup "main" globalEnvironment)
        exitCode <- int32 0
        ret exitCode
    insertClosure (name, closureEntry) = do
        let newStruct = ConstantOperand (Undef closureType)
        struct <- insertValue newStruct closureEntry [0]
        return (name, struct)

enter :: MonadIRBuilder m => Operand -> m ()
enter closure = do
    f <- extractValue closure [0]
    call f []
    return ()

codegenGlobalEntrys :: (MonadModuleBuilder m) => Binding -> m Operand
codegenGlobalEntrys (Binding name _) =
    function (Name (fromString (name ++ "_entry"))) [] void (\_ -> retVoid)
