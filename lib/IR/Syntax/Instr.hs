module IR.Syntax.Instr where

import Data.Map (Map)
import qualified Data.Map as Map

import IR.TypeSystem
import IR.Syntax.Expr
import IR.Loc


data IRInstr label where
    Free :: IRExpr label 'AddrTy -> IRInstr label 

    Is ::
         IRName
      -> IRExpr label 'AddrTy 
      -> IRInstr label 


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

    Loc ::
         IRLoc label
      -> IRInstr label
      -> IRInstr label

deriving instance (Show label) => Show (IRInstr label)

instance LocStrippable (IRInstr label) where
    stripLoc (Loc _ instr) = stripLoc instr
    stripLoc instr = instr

instance (Eq label) => Eq (IRInstr label) where
    (==) (Free name) (Free name') = (name == name') 
    (==) (Is name expr) (Is name' expr') = (name == name') && (expr == expr') 
    (==) (Set name expr) (Set name' expr') = (name == name') && (castAndCompare expr expr') 
    (==) (Loc loc  instr) (Loc loc'  instr') = (loc == loc') && (instr == instr') 
    (==) 
        (JComp name  expr1  expr2  label) 
        (JComp name' expr1' expr2' label') 
        = (name == name') && (expr1 == expr1') && (expr2 == expr2') && (label == label') 
    (==) _ _ = False

type Module label = Map label [IRInstr label]

addLocToModule :: Module label -> Module label
addLocToModule = Map.mapWithKey addLocToScope

addLocToScope :: label -> [IRInstr label] -> [IRInstr label]
addLocToScope scopeLabel = zipWith maybeLoc $ map (IRLoc scopeLabel) [0..]
                           where maybeLoc _ instr@(Loc _ _) = instr 
                                 maybeLoc label instr = Loc label instr