module IR.Frontend.Haskell where

import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (set)

import IR.Syntax

type Register = Map CName (Maybe CIntVal)
data RuntimeState = RuntimeState {
    _register :: Register
}
makeLenses ''RuntimeState

initialState :: RuntimeState
initialState = RuntimeState Map.empty


data RuntimeException
    = UsingDanglingPointer
    | UsingUnassignedValue
    | MemoryLeak
    | Redeclaration
    | DeallocatingUnallocated
    | AssigningUndeclared
    deriving (Show, Eq, Ord)

newtype Runtime a = Runtime {
    unwrapRuntime :: ExceptT RuntimeException (State RuntimeState) a
} deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadState RuntimeState
    , MonadError RuntimeException)

checkLeaks :: Runtime a -> Runtime a
checkLeaks program = do
    value <- program
    remainingMemory <- uses register (/= Map.empty)
    when remainingMemory $
        throwError MemoryLeak
    return value






whetherMember :: 
     CName 
  -> Bool 
  -> RuntimeException 
  -> Runtime ()
whetherMember name failIfIn errorType = do
    isIn <- uses register $ Map.member name
    when (isIn == failIfIn) $
        throwError errorType




-- machine primitives
compile :: CProgram 'UnitTy -> Runtime ()
compile (Seq instructions) = 
    void $ mapM compile instructions

compile (Allocate name) = do
    whetherMember name True Redeclaration
    modifying register $ Map.insert name Nothing

compile (Free name) = do
    whetherMember name False DeallocatingUnallocated
    modifying register $ Map.delete name

compile (Set name expr) = do
    whetherMember name False AssigningUndeclared
    value <- evaluate expr
    modifying register $ 
        Map.insert name $
        Just value


evaluate :: CProgram 'IntTy -> Runtime CIntVal
evaluate (Cst val) = return val
evaluate (Var   var) = do
    maybeVal <- uses register $ Map.lookup var
    case maybeVal of 
        Just (Just x) -> return x
        Just _ -> throwError UsingUnassignedValue
        _      -> throwError UsingDanglingPointer

evaluate (BinOp op expr1 expr2) = do
    result1 <- evaluate expr1
    result2 <- evaluate expr2
    return $ toHaskOp op result1 result2



run :: Runtime a -> Either RuntimeException a
run program = 
    runIdentity $
    flip evalStateT initialState $
    runExceptT          $
    unwrapRuntime       $
    checkLeaks program 


compileAndRun :: CProgram 'UnitTy -> Either RuntimeException ()
compileAndRun = run . compile