module IR.Backend.Haskell.Compiler where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import qualified Data.Map as Map

import qualified IR.Backend.Static as Static
import IR.Backend.Haskell.State

checkLeaks :: Runtime label a -> Runtime label a
checkLeaks program = do
    value <- program
    remainingMemory <- uses register Map.keys
    when (not $ null remainingMemory) $ do
        throwError $ MemoryLeak remainingMemory
    return value



checkLink :: (Ord label) => Runtime label a -> Runtime label a 
checkLink prog = do
    main <- use mainModule
    Static.checkLink main $ throwError . UndefinedLabels

    prog
