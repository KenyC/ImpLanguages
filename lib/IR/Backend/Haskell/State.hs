module IR.Backend.Haskell.State where

import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import Text.Printf

import IR.Syntax

newtype IRAddr = IRAddr Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 
data IRAddrOffset = IRAddrOffset IRAddr IRInt deriving (Eq, Ord, Show)

data UntypedData =
    IntVal   IRInt
  | AddrVal  IRAddrOffset
  deriving (Show, Eq)
makePrisms ''UntypedData  

type VecVals = Vector (Maybe UntypedData)

type Memory   = Map IRAddr VecVals
type NameMap  = Map IRName IRAddrOffset
data RuntimeState label = RuntimeState  {
     _register     :: Memory
   , _nextFreeAddr :: IRAddr
   , _nameMap      :: NameMap
   , _mainModule   :: Module label
   , _logger       :: [String]
   , _outValue     :: Maybe IRInt
   , _nextInstr    :: [IRInstr label]
} 
-- deriving instance (Show label) => Show (RuntimeState label)
makeLenses ''RuntimeState

initialState :: Module label -> RuntimeState label
initialState moduleProg = RuntimeState {
     _register     = Map.empty 
   , _nextFreeAddr = 0
   , _nameMap      = Map.empty
   , _mainModule   = moduleProg
   , _logger       = []
   , _outValue     = Nothing
   , _nextInstr    = []
}


data RuntimeException label
    = UseUnallocated IRAddr                  -- ^  something was freed and then used in an expression or never allocated at all
    | UseUndefinedName IRName                -- ^  a name was used before it became associated with an address
    | UsingUnassignedValue IRAddrOffset      -- ^  something was allocated but not assigned and then used in an expression
    | MemoryLeak [IRAddr]                    -- ^  something hasn't be freed at the end of program
    -- | ReallocationBeforeFree IRName          -- ^  something was allocated twice
    | DeallocatingUnallocated IRAddrOffset   -- ^  "free" was called on something that wasn't allocated before 
    | AssigningUnallocated IRAddr            -- ^  something was assigned to a name that hasn't been allocated before
    | UndefinedLabels [label]                -- ^  there are jumps to labels not defined in modules 
    | OutOfRange IRAddrOffset Int            -- ^  there are jumps to labels not defined in modules 
    | FreeingInsideBlock IRAddrOffset        -- ^  trying to free "&a[1]" where "a" might be on the heap 
    | TyError 
    | ErrorAt (IRLoc label) (RuntimeException label)
    deriving (Eq, Ord, Show)

instance LocStrippable (RuntimeException label) where
    stripLoc (ErrorAt _ exception) = stripLoc exception 
    stripLoc exception = exception

newtype Runtime label a = Runtime {
    unwrapRuntime :: 
        ExceptT 
            (RuntimeException label) 
            (State (RuntimeState label)) 
            a
} deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadState (RuntimeState     label)
    , MonadError (RuntimeException label))

maybeToError :: RuntimeException label -> Maybe a -> Runtime label a
maybeToError exception = maybe (throwError exception) return

newAddr :: Runtime label IRAddr
newAddr = do
    addr <- use nextFreeAddr
    modifying nextFreeAddr (+1)
    return addr