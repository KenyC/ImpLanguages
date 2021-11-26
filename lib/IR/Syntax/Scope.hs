module IR.Syntax.Scope where

import Data.Map (Map)

import IR.TypeSystem
import IR.Syntax.Expr

data IRScope label where
    -- Allocation
    Allocate :: IRName -> IRExpr label 'IntTy -> IRScope label 
    Free     :: IRName -> IRScope label 

    -- Set
    Set ::
        (IsTy ty)
     => IRExpr label 'AddrTy
     -> IRExpr label ty
     -> IRScope label 

    -- Control structures
    JComp ::
         IRCompOp
      -> IRExpr label 'IntTy
      -> IRExpr label 'IntTy
      -> label
      -> IRScope label 


deriving instance (Eq label)   => Eq   (IRExpr label a)
deriving instance (Show label) => Show (IRExpr label a)

type Module label = Map label [IRScope label]
