module IR.Backend.Asm where

import Control.Monad
import Data.Default
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map

import CodeGen.X86
import CodeGen.X86.Asm

import IR.Syntax
import IR.Backend.Asm.RunCode

compile :: 
    ( Default label
    , Ord     label
    , Show    label)
 => Module label 
 -> Maybe Code
compile mainModule = do
    scope <- Map.lookup def mainModule 
    return $ forM_ scope compileInstr

-- newtype AsmGen = AsmGen {
--     _unwrapAsmGen :: 
-- } deriving 
--     ( Functor
--     , Applicative
--     , Monad
--     , )

compileInstr :: IRInstr label -> Code
compileInstr = _
-- compileInstr (Set _ _) = _
-- compileInstr (Free _)        = _
-- compileInstr (Is _ _)        = _
-- compileInstr (JComp _ _ _ _) = _
-- compileInstr (Loc _ _)       = _


type StackPos = Word64
type NameMap  = Map IRName StackPos

compileExprInt :: NameMap -> IRExpr 'IntTy -> Code
compileExprInt _ (Cst x)   = mov rax (fromIntegral x)
compileExprInt nameMap (Deref addrExpr) = do
    compileExprAddr nameMap addrExpr
    mov rax $ addr64 rax
compileExprInt nameMap (BinOp op expr1 expr2) = do
    compileExprInt nameMap expr2 
    mov rcx rax
    compileExprInt nameMap expr1 
    toAsmOp op rax rcx 


compileExprAddr :: NameMap -> IRExpr 'AddrTy -> Code
compileExprAddr nameMap (Deref _) = do
    mov rax $ addr64 rax
compileExprAddr _ (Offset _ _) = _Offset
compileExprAddr nameMap (Var name) = do
    let stackPos = nameMap Map.! name
    mov rax $ addr64 $ rbp - (fromIntegral $ stackPos * sizeVar)
compileExprAddr _ (Allocate _) = _Allocate

newtype CompileExpr ty = CompileExpr {_unwrapCompileExpr :: NameMap -> IRExpr ty -> Code}
instance RecTy CompileExpr where
    int_  = CompileExpr compileExprInt
    addr_ = CompileExpr compileExprAddr

compileExpr :: (IsTy ty) => NameMap -> IRExpr ty -> Code
compileExpr = _unwrapCompileExpr impl

selfContainedExpr :: (IsTy ty) => NameMap -> IRExpr ty -> Code
selfContainedExpr nameMap expr = saveNonVolatile $ compileExpr nameMap expr

toAsmOp :: IRBinOp -> Operand 'RW 'S64 -> Operand r 'S64 -> Code
toAsmOp Add = add
toAsmOp Sub = sub
