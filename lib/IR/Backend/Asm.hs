module IR.Backend.Asm where

import Control.Monad
import Data.Default
import qualified Data.Map as Map

import CodeGen.X86

import IR.Syntax

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

compileExprInt :: IRExpr 'IntTy -> Code
compileExprInt expr = compileExprInt_ expr >> ret where
    compileExprInt_ :: IRExpr 'IntTy -> Code
    compileExprInt_ (Cst x)   = mov rax (fromIntegral x)
    compileExprInt_ (Deref _) = _
    compileExprInt_ (BinOp op expr1 expr2) = do
        compileExprInt_ expr2
        mov rcx rax
        compileExprInt_ expr1
        toAsmOp op rax rcx 

toAsmOp :: IRBinOp -> Operand 'RW 'S64 -> Operand r 'S64 -> Code
toAsmOp Add = add
toAsmOp Sub = sub
