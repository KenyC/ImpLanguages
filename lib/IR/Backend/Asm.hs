module IR.Backend.Asm where

import Control.Monad
import Data.Default
import Data.Word
import Data.Map    (Map)
import Data.Maybe  (fromJust)
import Data.List
import qualified Data.Map as Map

import CodeGen.X86
import CodeGen.X86.Asm
import CodeGen.X86.CodeGen
import CodeGen.X86.CallConv

import IR.Syntax
import IR.Backend.Asm.RunCode
import qualified IR.Backend.Static as Static

posOutValue :: StackPos
posOutValue = 1

data CompileError label
    = NoDefScope
    | UndefinedLabels [label]
    deriving (Show, Eq)

compile :: 
    ( Default label
    , Ord     label
    , Show    label)
 => Module label 
 -> Either (CompileError label) Code
compile mainModule = do
    when (not $ Map.member def mainModule) $ 
        Left NoDefScope 
    Static.checkLink mainModule $ Left . UndefinedLabels

    let unorderedScopes   = Map.toList mainModule

    -- placing 'def' first
    let (defScope:_, otherScopes) = partition ((== def) . fst) unorderedScopes
    let scopes = defScope:otherScopes

    let jumpMap  = JumpMap $ fst <$> scopes
    let nameMap  = makeNameMap mainModule

    let leaveProcedure = do
            mov rax (addrStackPos posOutValue)
            mov rsp rbp
            epilogue 
            ret

    return $ do
        prologue
        mov rbp rsp
        reserveSpaceForNames nameMap

        forM_ scopes $ \(_, scope) -> do
            -- start label
            mkCodeLine Label_
            mapM_ (compileInstr jumpMap nameMap) scope

            -- if the scope is over without jumps prepare to return
            leaveProcedure


makeNameMap :: Module label -> NameMap
makeNameMap mainModule = 
    Map.fromList   $
    -- all var's are stored 2 8-bytes after rbp, pos 1 is reserved for out value 
    flip zip [2..] $! -- not sure, seems like the best way to get early calculation of this massive thunk 
    nub            $ do
        scope <- Map.elems mainModule
        instr <- scope

        let collectNamesInstr :: IRInstr label -> [IRName]
            collectNamesInstr (Set expr1 expr2)        = collectNamesExpr expr1 ++ collectNamesExpr expr2 
            collectNamesInstr (Free expr1)             = collectNamesExpr expr1
            collectNamesInstr (Is name expr1)          = name:(collectNamesExpr expr1)
            collectNamesInstr (Out expr1)              = collectNamesExpr expr1
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

compileInstr :: (Eq label) => JumpMap label -> NameMap -> IRInstr label -> Code
-- compileInstr = _
compileInstr _ nameMap (Set exprAddr expr) = do
    compileExprAddr nameMap exprAddr
    push rax
    compileExpr nameMap expr
    pop rcx
    mov (addr64 rcx) rax

compileInstr _ nameMap (Is name exprAddr) = do
    compileExprAddr nameMap exprAddr
    let stackPos = nameMap Map.! name
    mov (addrStackPos stackPos) rax

compileInstr _ nameMap (Out exprVal) = do
    compileExpr nameMap exprVal
    mov (addrStackPos posOutValue) rax

compileInstr _ nameMap (Free exprAddr) = do
    compileExpr nameMap exprAddr
    mov arg1 rax -- move address to #1 arg (rdi on Linux)
    callFree rcx

compileInstr jMap nameMap (JComp op expr1 expr2 label) = do
    compileExpr nameMap expr1
    push rax -- saving result on stack
    compileExpr nameMap expr2
    pop rcx  -- popping first expression results from stack

    cmp rcx rax
    toAsmCompOp op $ 
        fromJust $
        jmapLookup label jMap



compileInstr jMap nameMap (Loc _ instr) = compileInstr jMap nameMap instr


type StackPos  = Word64
type NameMap   = Map IRName StackPos

newtype JumpMap l = 
    JumpMap {_unwrapJMap :: [l]}
    deriving (Eq, Show)
instance (Default l) => Default (JumpMap l) where
    def = JumpMap [def]
jmapLookup :: (Eq l) => l -> JumpMap l -> Maybe Label
jmapLookup key jmap = fmap Label $ elemIndex key $ _unwrapJMap jmap

makeJumpMap :: Module label -> JumpMap label
makeJumpMap = JumpMap . Map.keys


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


toAsmCompOp :: IRCompOp -> Label -> Code
toAsmCompOp Eq     = j E 
toAsmCompOp NEq    = j NE
toAsmCompOp More   = j G
toAsmCompOp MoreEq = j NLE


addrStackPos :: StackPos -> Operand r 'S64
addrStackPos pos = addr64 $ rbp - (fromIntegral $ pos * sizeVar)
