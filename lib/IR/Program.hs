module IR.Program where

import Control.Lens hiding ((*=))
import Control.Monad.State.Strict
import Data.Default
import qualified Data.Map as Map

import IR.TypeSystem
import IR.Syntax.Expr
import IR.Syntax.Instr

-- SYNTAX
data ProgramState label = ProgramState {
    _moduleProg   :: Module label,
    _currentLabel :: label,
    _nextName     :: IRName
}
makeLenses ''ProgramState

initialProgramState :: (Default label) => ProgramState label
initialProgramState = ProgramState {
    _moduleProg   = Map.empty,
    _currentLabel = def,
    _nextName     = 0
}


newtype IRProgram label a = IRProgram {
    unwrapProgram :: State (ProgramState label) a
} deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (ProgramState label) )

mkProg :: (Default label) => IRProgram label a -> Module label
mkProg = 
    addLocToModule  .
    view moduleProg .
    (flip execState initialProgramState) . 
    unwrapProgram



------------------- UTILS -----------------

addToLabel 
    :: (Ord label) 
    => IRInstr label  
    -> IRProgram label ()
addToLabel instruction = do
    label <- use currentLabel
    modifying moduleProg $ Map.insertWith (flip (++)) label [instruction]

newName :: IRProgram label IRName
newName = do 
    name <- use nextName
    modifying nextName (+1)
    return name

------------------- INSTRUCTIONS -----------------
allocate :: (Ord label) => IRName -> IRExpr 'IntTy -> IRProgram label ()
allocate name n = addToLabel $ Is name (Allocate n) 

allocateN :: (Ord label) => IRName -> IRInt -> IRProgram label ()
allocateN name n = allocate name $ Cst n 

allocate1 :: (Ord label) => IRName -> IRProgram label ()
allocate1 name = allocateN name 1 

allocate_ :: (Ord label) => IRExpr 'IntTy -> IRProgram label IRName
allocate_ n = do
    name <- newName
    allocate name n
    return name

allocateN_ :: (Ord label) => IRInt -> IRProgram label IRName
allocateN_ n = allocate_ $ Cst n

allocate1_ :: (Ord label) => IRProgram label IRName
allocate1_ = allocateN_ 1

free :: (Ord label) => IRExpr 'AddrTy -> IRProgram label ()
free expr = addToLabel $ Free expr 

jump :: (Ord label) => label -> IRProgram label ()
jump label = jcomp Eq (Cst 0) (Cst 0) label 

jcomp :: 
    (Ord label) 
 => IRCompOp 
 -> IRExpr 'IntTy 
 -> IRExpr 'IntTy 
 -> label 
 -> IRProgram label ()
jcomp op expr1 expr2 label = addToLabel $ JComp op expr1 expr2 label  


jeq :: 
    (Ord label) 
 => IRExpr 'IntTy 
 -> IRExpr 'IntTy 
 -> label 
 -> IRProgram label ()
jeq = jcomp Eq  

jneq :: 
    (Ord label) 
 => IRExpr 'IntTy 
 -> IRExpr 'IntTy 
 -> label 
 -> IRProgram label ()
jneq = jcomp NEq  



infix 4 *=
(*=) 
    :: (Ord label, IsTy ty)
    => IRExpr 'AddrTy
    -> IRExpr ty
    -> IRProgram label ()
(*=) name expr = addToLabel $ Set name expr

infix 4 .=
(.=) 
    :: (Ord label, IsTy ty)
    => IRName
    -> IRExpr ty
    -> IRProgram label ()
(.=) name expr = Var name *= expr


(*.) :: (IsTy ty) => IRName -> IRExpr ty
(*.) = Deref . Var 

(~>) :: label -> IRProgram label () -> IRProgram label ()
(~>) label scope = do 
    oldLabel <- use currentLabel
    assign currentLabel label
    scope
    assign currentLabel oldLabel






