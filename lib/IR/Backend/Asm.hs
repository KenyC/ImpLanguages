module IR.Backend.Asm where

import Control.Monad
import Data.Default
import Data.Word
import Data.Map  (Map)
import Data.List (nub)
import qualified Data.Map as Map

import CodeGen.X86

import IR.Syntax
import IR.Backend.Asm.RunCode

-- compile :: 
--     ( Default label
--     , Ord     label
--     , Show    label)
--  => Module label 
--  -> Maybe Code
-- compile mainModule = do
--     scope <- Map.lookup def mainModule 
--     return $ forM_ scope compileInstr

makeNameMap :: Module label -> NameMap
makeNameMap mainModule = 
    Map.fromList   $ 
    flip zip [1..] $! -- not sure, seems like the best way to get early calculation of this massive thunk 
    nub            $ do
        scope <- Map.elems mainModule
        instr <- scope

        let collectNamesInstr :: IRInstr label -> [IRName]
            collectNamesInstr (Set expr1 expr2)        = collectNamesExpr expr1 ++ collectNamesExpr expr2 
            collectNamesInstr (Free expr1)             = collectNamesExpr expr1
            collectNamesInstr (Is name expr1)          = name:(collectNamesExpr expr1)
            collectNamesInstr (JComp _ expr1 expr2 _)  = collectNamesExpr expr1 ++ collectNamesExpr expr2 
            collectNamesInstr (Loc _ instr)            = collectNamesInstr instr

            collectNamesExpr :: IRExpr ty -> [IRName]
            collectNamesExpr (Deref expr)          = collectNamesExpr expr
            collectNamesExpr (Allocate expr)       = collectNamesExpr expr
            collectNamesExpr (Offset expr1 expr2)  = collectNamesExpr expr1 ++ collectNamesExpr expr2 
            collectNamesExpr (Cst _)               = []
            collectNamesExpr (BinOp _ expr1 expr2) = collectNamesExpr expr1 ++ collectNamesExpr expr2 
            collectNamesExpr (Var var)             = [var]


        collectNamesInstr instr

reserveSpaceForNames :: NameMap -> Code
reserveSpaceForNames nameMap = do
    when (not $ Map.null nameMap) $ do
        let maxStackPos = (maximum nameMap + 1) * sizeVar
        sub rsp $ fromIntegral maxStackPos

-- newtype AsmGen = AsmGen {
--     _unwrapAsmGen :: 
-- } deriving 
--     ( Functor
--     , Applicative
--     , Monad
--     , )

compileInstr :: NameMap -> IRInstr label -> Code
-- compileInstr = _
compileInstr nameMap (Set exprAddr expr) = do
    compileExprAddr nameMap exprAddr
    push rax
    compileExpr nameMap expr
    pop rcx
    mov (addr64 rcx) rax

compileInstr nameMap (Is name exprAddr) = do
    compileExprAddr nameMap exprAddr
    let stackPos = nameMap Map.! name
    mov (addrStackPos stackPos) rax

compileInstr nameMap (Free exprAddr) = do
    compileExpr nameMap exprAddr
    mov arg1 rax -- move address to #1 arg (rdi on Linux)
    callFree rcx

compileInstr nameMap (JComp _ _ _ _) = _JComp
compileInstr nameMap (Loc _ instr) = compileInstr nameMap instr


type StackPos = Word64
type NameMap  = Map IRName StackPos

compileExprInt :: NameMap -> IRExpr 'IntTy -> Code
compileExprInt _ (Cst x)   = mov rax (fromIntegral x)

compileExprInt nameMap (Deref addrExpr) = do
    compileExprAddr nameMap addrExpr
    mov rax $ addr64 rax

compileExprInt nameMap (BinOp op expr1 expr2) = do
    compileExprInt nameMap expr1 
    push rax
    compileExprInt nameMap expr2 
    mov rcx rax
    pop rax
    toAsmOp op rax rcx 


compileExprAddr :: NameMap -> IRExpr 'AddrTy -> Code
compileExprAddr nameMap (Deref exprAddr) = do
    compileExprAddr nameMap exprAddr
    mov rax $ addr64 rax
compileExprAddr nameMap (Offset exprAddr exprOffset) = do
    compileExprAddr nameMap exprAddr
    push rax
    compileExprInt nameMap exprOffset
    shl rax 3 -- multiply by 8, so as to get to 64 bits-aligned address
    pop rcx
    add rax rcx


compileExprAddr nameMap (Var name) = do
    let stackPos = nameMap Map.! name
    mov rax $ addr64 $ rbp - (fromIntegral $ stackPos * sizeVar)

compileExprAddr nameMap (Allocate exprInt) = do
    compileExprInt nameMap exprInt
    mov arg1 rax
    shl arg1 3    -- * sizeof(IRInt) = * 8 bytes
    callMalloc rcx

newtype CompileExpr ty = CompileExpr {_unwrapCompileExpr :: NameMap -> IRExpr ty -> Code}
instance RecTy CompileExpr where
    int_  = CompileExpr compileExprInt
    addr_ = CompileExpr compileExprAddr

compileExpr :: (IsTy ty) => NameMap -> IRExpr ty -> Code
compileExpr = _unwrapCompileExpr impl

makeSelfContained :: NameMap -> Code -> Code
makeSelfContained nameMap body = saveNonVolatile $ do
    mov rbp rsp
    reserveSpaceForNames nameMap

    body
    mov rsp rbp

selfContainedExpr :: (IsTy ty) => NameMap -> IRExpr ty -> Code
selfContainedExpr nameMap expr = 
    makeSelfContained nameMap $ 
    compileExpr nameMap expr

toAsmOp :: IRBinOp -> Operand 'RW 'S64 -> Operand r 'S64 -> Code
toAsmOp Add = add
toAsmOp Sub = sub


addrStackPos :: StackPos -> Operand r 'S64
addrStackPos pos = addr64 $ rbp - (fromIntegral $ pos * sizeVar)
