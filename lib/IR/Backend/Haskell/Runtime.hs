module IR.Backend.Haskell.Runtime where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Vector ((//))
import Data.Proxy

import IR.Backend.Haskell.State
import IR.Backend.Haskell.Evaluate
import IR.Syntax


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

runtime :: (Ord label) => IRInstr label -> Runtime label ()
runtime (Set exprAddr exprVal) = do 
    full@(IRAddrOffset addr offset) <- evaluate exprAddr
    value <- evaluate exprVal
    vals  <- maybeToError (AssigningUnallocated addr) =<< (use (register . (at addr))) 

    let nVals = length vals
    let idx   = toIndex offset
    when (idx >= nVals) $
        throwError $ OutOfRange full nVals

    modifying register $ 
        Map.insert addr $ 
            vals // [(idx, Just $ toData (Proxy `asType` exprVal) value)]

    
runtime (Is name addrExpr) = do
    fullAddr <- evaluate addrExpr
    modifying nameMap $ Map.insert name fullAddr


runtime (Free addrExpr)     = do
    fullAddr@(IRAddrOffset addr offset) <- evaluateAddr addrExpr
    addrIsAllocated <- uses register (Map.member addr)

    when (not addrIsAllocated) $
        throwError $ DeallocatingUnallocated fullAddr
    when (offset /= 0) $
        throwError $ FreeingInsideBlock fullAddr

    modifying register $ Map.delete addr

runtime (JComp op expr1 expr2 label) = do
    doJump <- (toHaskCompOp op) <$> (evaluate expr1) <*> (evaluate expr2) 
    when doJump $ do
        -- Under the assumption that proper linking was checked before runtime
        instrs <- maybeToError (UndefinedLabels [label])  =<< (uses mainModule $ Map.lookup label)
        assign nextInstr $ instrs

runtime (Loc loc instr) = 
    catchError 
        (runtime instr)
        (\e -> throwError $ ErrorAt loc e)
