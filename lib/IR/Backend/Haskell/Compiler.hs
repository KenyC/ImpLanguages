module IR.Backend.Haskell.Compiler where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import qualified Data.Map as Map

import IR.Backend.Haskell.State
import IR.Syntax

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
    -> IRInstr label 
    -> [label]
checkLinkInScope mainModule scope = case scope of
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
