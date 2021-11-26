module IR.Syntax.Expr where

import Text.Printf

import IR.TypeSystem

newtype IRName   = IRName Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 
newtype IRInt    = IRInt  Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 

data IRBinOp
    = Add
    | Sub
    deriving (Eq, Show)
toHaskOp Add = (+)
toHaskOp Sub = (-)

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

data IRExpr label exprTy where
    -- Expressions
    Deref ::
         (IsTy ty)
      => IRExpr label 'AddrTy
      -> IRExpr label ty

    Offset ::
         IRExpr label 'AddrTy
      -> IRExpr label 'IntTy
      -> IRExpr label 'AddrTy

    Cst :: 
         IRInt
      -> IRExpr label 'IntTy

    BinOp :: 
         IRBinOp
      -> IRExpr label 'IntTy
      -> IRExpr label 'IntTy
      -> IRExpr label 'IntTy

    Var :: 
         IRName
      -> IRExpr label 'AddrTy
