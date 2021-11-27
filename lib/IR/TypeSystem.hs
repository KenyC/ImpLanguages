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


newtype CastToInt f (ty :: IRType) = CastToInt {
     _unwrapCastToInt :: f ty -> Maybe (f 'IntTy)
}
castToInt :: (IsTy ty) => f ty -> Maybe (f 'IntTy)
castToInt = _unwrapCastToInt impl
instance RecTy (CastToInt f) where
     int_  = CastToInt $ Just
     addr_ = CastToInt $ const Nothing

newtype CastToAddr f (ty :: IRType) = CastToAddr {
     _unwrapCastToAddr :: f ty -> Maybe (f 'AddrTy)
}
castToAddr :: (IsTy ty) => f ty -> Maybe (f 'AddrTy)
castToAddr = _unwrapCastToAddr impl
instance RecTy (CastToAddr f) where
     int_  = CastToAddr $ const Nothing
     addr_ = CastToAddr $ Just

newtype Castable f (ty1 :: IRType) (ty2 :: IRType) = Castable {
     _unwrapCastable :: f ty1 -> Maybe (f ty2)
}
cast :: (IsTy ty1, IsTy ty2) => f ty1 -> Maybe (f ty2)
cast = _unwrapCastable impl
instance (IsTy ty1) => RecTy (Castable f ty1) where
     int_  = Castable castToInt
     addr_ = Castable castToAddr
