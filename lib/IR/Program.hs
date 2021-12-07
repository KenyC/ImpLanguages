module IR.Program where

import Control.Lens hiding ((*=))
import Control.Monad.State.Strict
import Data.Default
import Data.Maybe   (fromMaybe)
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

mkScope :: (Default label, Ord label) => IRProgram label a -> [IRInstr label]
mkScope =
    fromMaybe []                         .
    Map.lookup def                       .
    view moduleProg                      .
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

------------------- CONVENIENCE POLYMORPHISM -----------------

class LiftToIntExpr a where
    liftToExpr :: a -> IRExpr 'IntTy

instance LiftToExpr (IRExpr 'IntTy) where
    liftToExpr = id

instance LiftToExpr IRInt where 
    liftToExpr = Cst

instance LiftToExpr 'AddrTy IRName where 
    liftToExpr = Var

------------------- INSTRUCTIONS -----------------
allocate :: 
    (Ord label, LiftToExpr 'IntTy a) 
 => IRName -> a -> IRProgram label ()
allocate name n = addToLabel $ Is name (Allocate $ liftToExpr n) 


allocate1 :: (Ord label) => IRName -> IRProgram label ()
allocate1 name = allocate name (1 :: IRInt) 

allocate_ :: 
     (Ord label, LiftToExpr 'IntTy a) 
  => a -> IRProgram label IRName
allocate_ n = do
    name <- newName
    allocate name n
    return name

allocate1_ :: (Ord label) => IRProgram label IRName
allocate1_ = allocate_ (1 :: IRInt)

is :: 
     (Ord label, LiftToExpr 'AddrTy a) 
  => IRName -> a -> IRProgram label () 
is name expr = addToLabel $ Is name $ liftToExpr expr

free :: 
    (Ord label, LiftToExpr 'AddrTy a) 
 => a -> IRProgram label ()
free expr = addToLabel $ Free $ liftToExpr expr 

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






