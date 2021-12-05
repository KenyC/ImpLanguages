module IR.Backend.Haskell.Evaluate where

import Control.Lens
import qualified Data.Map as Map
import Data.Proxy
import Data.Vector ((!?))
import qualified Data.Vector as Vector

import IR.Backend.Haskell.State
import IR.Syntax

type family   ValueOf (ty :: IRType)
type instance (ValueOf 'IntTy)  = IRInt
type instance (ValueOf 'AddrTy) = IRAddrOffset

newtype HasPrism (ty :: IRType) = HasPrism {
    _unwrapPrism :: Prism' UntypedData (ValueOf ty)
}
instance RecTy HasPrism where
    int_  = HasPrism _IntVal
    addr_ = HasPrism _AddrVal
fromData :: (IsTy ty) => Proxy ty -> UntypedData -> Maybe (ValueOf ty)
fromData proxy = preview (_unwrapPrism $ impl `asType` proxy)
toData :: (IsTy ty) => Proxy ty -> (ValueOf ty) -> UntypedData
toData proxy = review (_unwrapPrism $ impl `asType` proxy)



newtype Evaluable label ty = Evaluable {
    _unwrapEvaluable :: IRExpr ty -> Runtime label (ValueOf ty) 
}

toIndex :: IRInt -> Int
toIndex (IRInt x) = x 


evaluateInt :: IRExpr 'IntTy -> Runtime label (ValueOf 'IntTy)
evaluateInt (Deref addrExpr) = derefAddr @'IntTy Proxy addrExpr
evaluateInt (Cst   value)    = return value
evaluateInt (BinOp op expr1 expr2) = 
    (toHaskOp op) <$> (evaluateInt expr1) <*> (evaluateInt expr2)

derefAddr :: (IsTy ty) => Proxy ty -> IRExpr 'AddrTy -> Runtime label (ValueOf ty)
derefAddr proxy addrExpr = do
    fullAddr@(IRAddrOffset addr offset) <- evaluateAddr addrExpr
    vals      <- maybeToError (UseUnallocated addr)  =<< uses register (Map.lookup addr)
    maybeVal  <- maybeToError (OutOfRange fullAddr (length vals)) $
                    vals !? (toIndex offset)
    val <- maybeToError (UsingUnassignedValue fullAddr) maybeVal
    maybeToError (TyError) $ fromData proxy val 


evaluateAddr :: IRExpr 'AddrTy -> Runtime label (ValueOf 'AddrTy)
evaluateAddr (Allocate intExpr) = do
    size_ <- evaluateInt intExpr
    let size = toIndex size_
    addr <- newAddr
    modifying register $ Map.insert addr (Vector.replicate size Nothing)
    return $ IRAddrOffset addr 0

evaluateAddr (Deref  addrExpr) = derefAddr @'AddrTy Proxy addrExpr
    
evaluateAddr (Offset addrExpr offsetExpr) = do
    IRAddrOffset addr offset <- evaluateAddr addrExpr
    offset' <- evaluateInt offsetExpr
    return $ IRAddrOffset addr (offset + offset')

evaluateAddr (Var name) = do
    maybeAddr <- uses nameMap $ Map.lookup name 
    addr <- maybeToError (UseUndefinedName name) maybeAddr
    return $ addr

evaluate :: (IsTy ty) => IRExpr ty -> Runtime label (ValueOf ty)
evaluate = _unwrapEvaluable impl
instance RecTy (Evaluable label) where
    int_  = Evaluable evaluateInt
    addr_ = Evaluable evaluateAddr