module IR.Syntax.Instr where

import Data.Map (Map)
import qualified Data.Map as Map

import Pretty
import IR.TypeSystem
import IR.Syntax.Expr
import IR.Loc

data IRCompOp
    = Eq
    | NEq
    | More
    | MoreEq
    deriving (Eq, Show)
toHaskCompOp Eq     = (==) 
toHaskCompOp NEq    = (/=) 
toHaskCompOp More   = (>) 
toHaskCompOp MoreEq = (>=) 

instance Pretty IRCompOp where
    prettyShowPrec Eq     = ("==" , 10) 
    prettyShowPrec NEq    = ("/=" , 10) 
    prettyShowPrec More   = (">"  , 10) 
    prettyShowPrec MoreEq = (">=" , 10) 


data IRInstr label where
    Free :: IRExpr 'AddrTy -> IRInstr label 

    -- returns value
    Out :: 
         IRExpr 'IntTy 
      -> IRInstr label

    Is ::
         IRName
      -> IRExpr 'AddrTy 
      -> IRInstr label 


    -- Set
    Set ::
        (IsTy ty)
     => IRExpr 'AddrTy
     -> IRExpr ty
     -> IRInstr label 

    -- Control structures
    JComp ::
         IRCompOp
      -> IRExpr 'IntTy
      -> IRExpr 'IntTy
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

instance (Show label) => Pretty (IRInstr label) where
    prettyShowPrec (Is name addr) = ((prettyShow name) ++ " := " ++ (prettyShow addr), 10)
    prettyShowPrec (Free expr)       = ("free " ++ (prettyShow expr), 10)
    prettyShowPrec (Set  addr value) = ((prettyShow addr) ++ " *= " ++ (prettyShow value), 10)
    prettyShowPrec (JComp op expr1 expr2 label) = ("jump if (" ++ (prettyShow expr1) ++") " ++ prettyShow op ++ " (" ++ (prettyShow expr2) ++ ") " ++ (show label), 10)  
    prettyShowPrec (Out expr) = ("out " ++ prettyShow expr, 10)
    prettyShowPrec (Loc _ instr) = prettyShowPrec instr




type Module label = Map label [IRInstr label]

addLocToModule :: Module label -> Module label
addLocToModule = Map.mapWithKey addLocToScope

addLocToScope :: label -> [IRInstr label] -> [IRInstr label]
addLocToScope scopeLabel = zipWith maybeLoc $ map (IRLoc scopeLabel) [0..]
                           where maybeLoc _ instr@(Loc _ _) = instr 
                                 maybeLoc label instr = Loc label instr

instance (Show label) => Pretty (Module label) where
    prettyShowPrec mainModule = 
        flip (,) 10 $
            unlines 
            [ unlines $ ("[" ++ (show scopeLabel) ++ "]:"):(map (tabulate . prettyShow) scope)
            | (scopeLabel, scope) <- Map.toList mainModule]    
        where tabulate x = '\t':x 
