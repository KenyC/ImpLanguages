module Machine where

import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (set)
import Types

newtype Addr = Addr {
    _addr :: Int
} deriving (Eq, Ord, Show, Num)

type Register = Map Addr (Maybe Val)
data MachineState = MachineState {
    _register :: Register,
    _maxCount :: Addr
}
makeLenses ''MachineState

initialState :: MachineState
initialState = MachineState Map.empty 0


data Bug
    = SegFault
    | UsingUnassignedValue
    | NotDeallocated
    | DeallocatingUnallocated
    deriving (Show, Eq, Ord)

newtype Machine a = Machine {
    unwrapMachine :: ExceptT Bug (State MachineState) a
} deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadState MachineState
    , MonadError Bug)

checkIsIn :: Addr -> Bug -> Machine ()
checkIsIn addr errorType = do
    isIn <- uses register $ Map.notMember addr
    when isIn $
        throwError errorType



-- machine primitives

allocate :: Machine Addr
allocate = do
    newIndex <- use maxCount
    modifying maxCount (+1)
    modifying register $ Map.insert newIndex Nothing 
    return newIndex

deallocate :: Addr -> Machine ()
deallocate addr = do
    checkIsIn addr DeallocatingUnallocated
    modifying register $ Map.delete addr


val :: Addr -> Machine Val
val addr = do
    maybeValue <- uses register $ Map.lookup addr
    case maybeValue of 
        Nothing    -> throwError SegFault
        Just value -> maybe 
                        (throwError UsingUnassignedValue) 
                        return 
                        value

set :: Addr -> Val -> Machine () 
set addr value = do
    checkIsIn addr UsingUnassignedValue
    modifying register $ Map.insert addr $ Just value


if_ :: Val -> Machine () -> Machine ()
if_ condition scope = 
    when (condition /= 0) scope

-- RUN & CHECK PROGRAMS

checkUnallocated :: Machine a -> Machine a
checkUnallocated program = do
    value <- program
    remainingMemory <- uses register (/= Map.empty)
    when remainingMemory $
        throwError NotDeallocated
    return value

run :: Machine a -> Either Bug a
run program = 
    runIdentity $
    flip evalStateT initialState $
    runExceptT          $
    unwrapMachine       $
    checkUnallocated program 





goodProgram :: Machine ()
goodProgram = do
    a <- allocate
    set a 0
    deallocate a


goodProgram1 :: Machine Val
goodProgram1 = do

    b <- allocate 
    set b 23 

    c <- allocate 
    set c 24 

    a <- allocate
    let sumBC =  return (+) <*> (val b) <*> (val c) 
    sumBC >>= set a 
    toReturn <- val a    

    deallocate a
    deallocate b
    deallocate c

    return toReturn



badProgram :: Machine ()
badProgram = do
    a <- allocate
    set a 0
    -- forgetting to deallocate



badProgram1 :: Machine ()
badProgram1 = do
    a <- allocate
    b <- allocate
    -- giving a an undefined value
    val b >>= set a
    deallocate a
    deallocate b




badProgram2 :: Machine ()
badProgram2 = do
    a <- allocate
    set a 0
    deallocate a

    -- Using a after it has been deallocated
    b <- allocate
    set b 0
    val a >>= set b
    deallocate b


