module IR.Backend.Haskell where

import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Printf
import Data.Default
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (set)

import IR.Syntax

newtype CAddr = CAddr Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 

type Memory   = Map CAddr (Maybe CIntVal)
type NameMap  = Map CName CAddr
data RuntimeState scopeLabel = RuntimeState  {
     _register     :: Memory
   , _nextFreeAddr :: CAddr
   , _nameMap      :: NameMap
   , _mainModule   :: Module scopeLabel
   , _logger       :: [String]
   , _nextInstr    :: [CScope scopeLabel 'UnitTy]
} deriving (Show)
makeLenses ''RuntimeState

initialState :: Module scopeLabel -> RuntimeState scopeLabel
initialState moduleProg = RuntimeState {
     _register     = Map.empty 
   , _nextFreeAddr = 0
   , _nameMap      = Map.empty
   , _mainModule   = moduleProg
   , _logger       = []
   , _nextInstr    = []
}


data RuntimeException label
    = UseUnallocated CName            -- ^  something was freed and then used in an expression or never allocated at all
    | UsingUnassignedValue CName      -- ^  something was allocated but not assigned and then used in an expression
    | MemoryLeak [CAddr]              -- ^  something hasn't be freed at the end of program
    -- | Redeclaration CName             -- ^  something was allocated twice
    | DeallocatingUnallocated CName   -- ^  "free" was called on something that wasn't allocated before 
    | AssigningUnallocated CName      -- ^  something was assigned to a name that hasn't been allocated before
    | UndefinedLabels [label]         -- ^  there are jumps to labels not defined in modules 
    deriving (Show, Eq, Ord)

newtype Runtime label a = Runtime {
    unwrapRuntime :: ExceptT (RuntimeException label) (State (RuntimeState label)) a
} deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadState (RuntimeState     label)
    , MonadError (RuntimeException label))

checkLeaks :: Runtime label a -> Runtime label a
checkLeaks program = do
    value <- program
    remainingMemory <- uses register Map.keys
    when (not $ null remainingMemory) $ do
        throwError $ MemoryLeak remainingMemory
    return value

checkLinkInScope 
    :: (Ord label) 
    => Module label 
    -> CScope label 'UnitTy 
    -> [label]
checkLinkInScope mainModule scope = case scope of
    Jump l         -> if l `Map.member` mainModule then [] else [l]
    JComp _ _ _ l  -> if l `Map.member` mainModule then [] else [l]
    _              -> []

checkLink :: (Ord label) => Runtime label a -> Runtime label a 
checkLink prog = do
    main <- use mainModule
    let labels = mconcat $ [ checkLinkInScope main instruction 
                           | instructions <- Map.elems main 
                           , instruction  <- instructions   ]

    when (not $ null labels) $
        throwError $ UndefinedLabels labels
    prog

withLabel 
    :: (Ord label)
    => label
    -> ([CScope label 'UnitTy] -> Runtime label a)
    -> Runtime label a
withLabel label action = do
    maybeScope <- Map.lookup label <$> use mainModule
    maybe
        (throwError $ UndefinedLabels [label])
        action
        maybeScope

withAddr 
    :: (Ord label)
    => CName
    -> RuntimeException label
    -> (CAddr -> Runtime label a)
    -> Runtime label a
withAddr name exception action = do
    maybeAddr <- uses nameMap $ Map.lookup name
    case maybeAddr of
        Nothing   -> throwError exception
        Just addr -> action addr


writeToLog :: String -> Runtime label ()
writeToLog line = modifying logger $ (line:)

newAddr :: Runtime label CAddr
newAddr = do
    freeAddr <- use nextFreeAddr
    modifying nextFreeAddr (+1)
    modifying register $ Map.insert freeAddr Nothing
    return freeAddr 

whetherMember :: 
     CName 
  -> Bool 
  -> RuntimeException label
  -> Runtime label ()
whetherMember name failIfIn errorType = do
    isIn <- uses nameMap $ Map.member name
    when (isIn == failIfIn) $
        throwError errorType


runtimeStep :: (Show label, Ord label) => Runtime label ()
runtimeStep = do
    maybeUncons <- uses nextInstr $ uncons
    case maybeUncons of 
        Nothing            -> return ()
        Just (instr, rest) -> do
            assign nextInstr rest
            runtime instr

runTillEnd :: (Show label, Ord label) => Runtime label ()
runTillEnd = do
    isNotEnd <- uses nextInstr $ not . null
    when isNotEnd $ do 
        runtimeStep
        runTillEnd

runtime :: (Show label, Ord label) => CScope label 'UnitTy -> Runtime label () 
runtime Pass = return ()
runtime (Allocate name) = do
    writeToLog $ printf "allocate %s" (show name)
    addr <- newAddr
    modifying nameMap $ Map.insert name addr

runtime (Free name) = 
    withAddr name (DeallocatingUnallocated name) $ \addr -> do
        writeToLog $ printf "free %s" (show name)
        addrIsAllocated <- uses register $ Map.member addr
        when (not addrIsAllocated) $ do
            throwError $ DeallocatingUnallocated name
        modifying register $ Map.delete addr



runtime (Set name expr) = 
    withAddr name (AssigningUnallocated name) $ \addr -> do
        value <- evaluate expr
        writeToLog $ printf "%s <- %s (= %s)" (show name) (show expr) (show value)
        modifying register $ 
            Map.insert addr $
            Just value


runtime (Jump label) = 
    withLabel label $ \scope -> do
        writeToLog $ printf "jump %s" (show label) 
        assign nextInstr scope

runtime (JComp op a b label) = withLabel label $ \scope -> do 
    valA <- evaluate a
    valB <- evaluate b
    let compVal = toHaskCompOp op valA valB
    writeToLog $ 
        printf "jcomp %s %s %s %s >> %s" 
        (show op) 
        (show a) (show b)
        (show label) (show compVal)
    when compVal $ 
        assign nextInstr scope


evaluate :: (Ord label) => CScope label 'IntTy -> Runtime label CIntVal
evaluate (Cst  val)  = return val
evaluate (Var  name) = 
    withAddr name (UseUnallocated name)  $ \addr -> do
        maybeVal <- uses register $ Map.lookup addr
        case maybeVal of 
            Just (Just x) -> return x
            Just _ -> throwError (UsingUnassignedValue name)
            _      -> throwError (UseUnallocated       name)

evaluate (BinOp op expr1 expr2) = do
    result1 <- evaluate expr1
    result2 <- evaluate expr2
    return $ toHaskOp op result1 result2



compileAndRun_ 
    :: (Default label, Ord label, Show label)
    => Module label 
    -> (Maybe (RuntimeException label), RuntimeState label)
compileAndRun_ mainModule = 
    mainModule                            &
    Map.lookup def                        & -- find main module
    maybe (return ()) (assign nextInstr)  & -- if existent, run ; otherwise run nothing
    checkLink                   & -- check links (all labels refer)
    flip (>>) runTillEnd        &
    checkLeaks                  & -- check leaks
    unwrapRuntime               & -- remove "newtype" layer
    runExceptT                  & -- run for exeptions
    flip runStateT (initialState mainModule) & -- run with initial state
    runIdentity                 & -- remove "newtype layer"
    over _1 (either Just (const Nothing))   -- if error, return error

compileAndRun :: 
    (Default label, Ord label, Show label)
 => Module label 
 -> Maybe (RuntimeException label)
compileAndRun = fst . compileAndRun_

-- run_ :: Runtime label a -> (Either (RuntimeException label) a, Register)
-- run_ program = 
--     over _2 _register $
--     runIdentity $
--     flip runStateT initialState $
--     runExceptT          $
--     unwrapRuntime       $
--     checkLeaks          $ 
--     checkLinkInScope    $ 
--     program 


-- runtimeAndRun :: CScope label 'UnitTy -> Either (RuntimeException label) ()
-- runtimeAndRun = run . runtime

-- runtimeAndRun_ :: CScope label 'UnitTy -> (Either (RuntimeException label) (), Register)
-- runtimeAndRun_ = run_ . runtime