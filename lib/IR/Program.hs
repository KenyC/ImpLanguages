module IR.Program where

import Control.Lens
import Control.Monad.State.Strict
import Data.Default
import qualified Data.Map as Map

import IR.TypeSystem
import IR.Syntax.Expr
import IR.Syntax.Scope

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
    view moduleProg .
    (flip execState initialProgramState) . 
    unwrapProgram



------------------- UTILS -----------------

addToLabel 
    :: (Ord label) 
    => IRScope label  
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
allocate :: (Ord label) => IRName -> IRExpr label 'IntTy -> IRProgram label ()
allocate name n = addToLabel $ Allocate name n 

allocate_ :: (Ord label) => IRExpr label 'IntTy -> IRProgram label IRName
allocate_ n = do
    name <- newName
    allocate name n
    return name

free :: (Ord label) => IRName -> IRProgram label ()
free name = addToLabel $ Free name 

jump :: (Ord label) => label -> IRProgram label ()
jump label = jcomp Eq (Cst 0) (Cst 0) label 

jcomp :: 
    (Ord label) 
 => IRCompOp 
 -> IRExpr label 'IntTy 
 -> IRExpr label 'IntTy 
 -> label 
 -> IRProgram label ()
jcomp op expr1 expr2 label = addToLabel $ JComp op expr1 expr2 label  


jeq :: 
    (Ord label) 
 => IRExpr label 'IntTy 
 -> IRExpr label 'IntTy 
 -> label 
 -> IRProgram label ()
jeq = jcomp Eq  

jneq :: 
    (Ord label) 
 => IRExpr label 'IntTy 
 -> IRExpr label 'IntTy 
 -> label 
 -> IRProgram label ()
jneq = jcomp NEq  


(|=) 
    :: (Ord label, IsTy ty)
    => IRExpr label 'AddrTy
    -> IRExpr label ty
    -> IRProgram label ()
(|=) name expr = addToLabel $ Set name expr

(.=) 
    :: (Ord label, IsTy ty)
    => IRName
    -> IRExpr label ty
    -> IRProgram label ()
(.=) name expr = Var name |= expr



(~>) :: label -> IRProgram label () -> IRProgram label ()
(~>) label scope = do 
    oldLabel <- use currentLabel
    assign currentLabel label
    scope
    assign currentLabel oldLabel






