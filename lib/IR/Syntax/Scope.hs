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

deriving instance (Show label) => Show (IRScope label)

instance (Eq label) => Eq (IRScope label) where
    (==) (Allocate name size) (Allocate name' size') = (name == name') && (size == size') 
    (==) (Free name) (Free name') = (name == name') 
    (==) (Set name expr) (Set name' expr') = (name == name') && (castAndCompare expr expr') 
    (==) 
        (JComp name  expr1  expr2  label) 
        (JComp name' expr1' expr2' label') 
        = (name == name') && (expr1 == expr1') && (expr2 == expr2') && (label == label') 
    (==) _ _ = False

type Module label = Map label [IRScope label]
