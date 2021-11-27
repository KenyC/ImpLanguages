module IR.Syntax.Instr where

import Data.Map (Map)

import IR.TypeSystem
import IR.Syntax.Expr

data IRInstr label where
    -- Allocation
    Allocate :: IRName -> IRExpr label 'IntTy -> IRInstr label 
    Free     :: IRName -> IRInstr label 

    -- Set
    Set ::
        (IsTy ty)
     => IRExpr label 'AddrTy
     -> IRExpr label ty
     -> IRInstr label 

    -- Control structures
    JComp ::
         IRCompOp
      -> IRExpr label 'IntTy
      -> IRExpr label 'IntTy
      -> label
      -> IRInstr label 

deriving instance (Show label) => Show (IRInstr label)

instance (Eq label) => Eq (IRInstr label) where
    (==) (Allocate name size) (Allocate name' size') = (name == name') && (size == size') 
    (==) (Free name) (Free name') = (name == name') 
    (==) (Set name expr) (Set name' expr') = (name == name') && (castAndCompare expr expr') 
    (==) 
        (JComp name  expr1  expr2  label) 
        (JComp name' expr1' expr2' label') 
        = (name == name') && (expr1 == expr1') && (expr2 == expr2') && (label == label') 
    (==) _ _ = False

type Module label = Map label [IRInstr label]
