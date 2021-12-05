module IR.Syntax.Expr where

import Text.Printf

import Pretty
import IR.TypeSystem

newtype IRName   = IRName Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 
newtype IRInt    = IRInt  Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg, Real, Integral) 

instance Pretty IRName where
     prettyShowPrec (IRName x) = ("$" ++ (show x), 10)

instance Pretty IRInt where
     prettyShowPrec (IRInt x) = (show x, 10)


data IRBinOp
    = Add
    | Sub
    deriving (Eq, Show)
toHaskOp Add = (+)
toHaskOp Sub = (-)

instance Pretty IRBinOp where
     prettyShowPrec Add = ("+", 10)
     prettyShowPrec Sub = ("-", 10)


data IRExpr exprTy where
    -- Expressions
    Deref ::
         (IsTy ty)
      => IRExpr 'AddrTy
      -> IRExpr ty

    Offset ::
         IRExpr 'AddrTy
      -> IRExpr 'IntTy
      -> IRExpr 'AddrTy

    Cst :: 
         IRInt
      -> IRExpr 'IntTy

    BinOp :: 
         IRBinOp
      -> IRExpr 'IntTy
      -> IRExpr 'IntTy
      -> IRExpr 'IntTy

    Var :: 
         IRName
      -> IRExpr 'AddrTy

    -- Allocation
    Allocate :: IRExpr 'IntTy  -> IRExpr 'AddrTy 

deriving instance Eq   (IRExpr a)
deriving instance Show (IRExpr a)

newtype PrettyShow label ty = PrettyShow {_unwrapPrettyShow :: IRExpr ty -> (String, Int)}

wrapInParen prec expr
      | prec < 8  = "(" ++ expr ++ ")"
      | otherwise = expr
prettyShowDeref exprAddr = let 
     (expr, prec) = prettyShowPrec exprAddr 
     in if | prec < 8  -> ("*(" ++ expr ++ ")", 10)
           | otherwise -> ("*" ++ expr, 10)
instance RecTy (PrettyShow label) where
     int_ = PrettyShow $ \case
          (Cst x)          -> prettyShowPrec x
          (Deref exprAddr) -> prettyShowDeref exprAddr
          (BinOp op x y)   -> let (exprX,  precX)  = prettyShowPrec x  
                                  (exprY,  precY)  = prettyShowPrec y 
                                  (exprOp, precOp) = prettyShowPrec op 
                              in (wrapInParen precX exprX ++ " " ++ exprOp ++ " " ++ wrapInParen precY exprY, 5)
     addr_ = PrettyShow $ \case
          (Deref exprAddr)     ->  prettyShowDeref exprAddr
          (Offset addr offset) -> let (exprAddr,   precAddr)   = prettyShowPrec addr  
                                      (exprOffset, precOffset) = prettyShowPrec offset  
                                  in (wrapInParen precAddr exprAddr ++ " + " ++ wrapInParen precOffset exprOffset, 5)
          (Var v)      -> prettyShowPrec v
          (Allocate n) -> ("allocate("++ prettyShow n ++")", 10) 


instance (IsTy ty) => Pretty (IRExpr ty) where
     prettyShowPrec = _unwrapPrettyShow impl


castAndCompare :: (IsTy ty1, IsTy ty2) => IRExpr ty1 -> IRExpr ty2 -> Bool
castAndCompare expr1 expr2 = (cast expr1) == (Just expr2)

infixl 6 .+.
(.+.) :: IRExpr 'IntTy -> IRExpr 'IntTy -> IRExpr 'IntTy
(.+.) = BinOp Add

infixl 6 .-.
(.-.) :: IRExpr 'IntTy -> IRExpr 'IntTy -> IRExpr 'IntTy
(.-.) = BinOp Sub