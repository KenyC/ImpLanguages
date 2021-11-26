module IR.Backend.Haskell where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Default
import qualified Data.Map as Map

import IR.Backend.Haskell.Runtime
import IR.Backend.Haskell.Compiler
import IR.Backend.Haskell.State
import IR.Syntax

compileAndRun_ 
    :: (Default label, Ord label, Show label)
    => Module label 
    -> (Maybe (RuntimeException label), RuntimeState label)
compileAndRun_ mainModule = 
    mainModule                               &
    Map.lookup def                           & -- find main module
    maybe (return ()) (assign nextInstr)     & -- if existent, run ; otherwise run nothing
    checkLink                                & -- check links (all labels refer)
    flip (>>) runTillEnd                     &
    checkLeaks                               & -- check leaks
    unwrapRuntime                            & -- remove "newtype" layer
    runExceptT                               & -- run for exceptions
    flip runStateT (initialState mainModule) & -- run with initial state
    runIdentity                              & -- remove "newtype layer"
    over _1 (either Just (const Nothing))   -- if error, return error

compileAndRun :: 
    ( Default label
    , Ord     label
    , Show    label)
 => Module label 
 -> Maybe (RuntimeException label)
compileAndRun = fst . compileAndRun_