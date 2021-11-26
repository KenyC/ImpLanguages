module IR.TypeSystem where

import Data.Proxy

data IRType
    = IntTy
    | AddrTy
    deriving (Eq, Show)

class IsTy (a :: IRType) where
    impl :: (RecTy f) => f a 

instance IsTy 'IntTy where
    impl = int_
instance IsTy 'AddrTy where
    impl = addr_

class RecTy (f :: IRType -> *) where
    int_   :: f 'IntTy
    addr_  :: f 'AddrTy

asType :: forall f g (a :: IRType). f a -> g a -> f a 
asType = const
