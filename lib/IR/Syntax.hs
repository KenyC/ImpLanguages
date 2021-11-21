module IR.Syntax where

import Control.Monad.Writer

{-
Language spec

int* val = allocate; // allocate val

*val = 23         // setVal 

skipneq *val

free(val);
-}

-- SYNTAX

data CType
    = IntTy
    | UnitTy
    deriving (Eq, Show)


data CBinOp
    = Add
    | Sub
    deriving (Eq, Show)

toHaskOp Add = (+)
toHaskOp Sub = (-)

newtype CName   = CName   Int deriving (Num, Eq, Show, Ord) 
newtype CIntVal = CIntVal Int deriving (Num, Eq, Show, Ord) 



data CProgram exprTy where
    -- Allocation
    Allocate :: CName -> CProgram 'UnitTy
    Free     :: CName -> CProgram 'UnitTy

    -- Expressions
    Cst :: 
         CIntVal
      -> CProgram 'IntTy

    Var :: 
         CName
      -> CProgram 'IntTy

    BinOp :: 
         CBinOp
      -> CProgram 'IntTy
      -> CProgram 'IntTy
      -> CProgram 'IntTy

    -- Set
    Set ::
        CName
     -> CProgram 'IntTy
     -> CProgram 'UnitTy

    -- Control structures
    Seq ::
         [CProgram 'UnitTy]
      ->  CProgram 'UnitTy



newtype CProgram_ a = CProgram_ {
    unwrapProgram :: Writer [CProgram 'UnitTy] a
} deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [CProgram 'UnitTy] )

--
-- Allocation
allocate :: CName -> CProgram_ ()
allocate name = tell [Allocate name] 

free     :: CName -> CProgram_ ()
free name = tell [Free name] 


(|=) :: CName
     -> CProgram 'IntTy
     -> CProgram_ ()
(|=) name expr = tell [Set name expr]


mkProg :: CProgram_ a -> CProgram 'UnitTy
mkProg = Seq . execWriter . unwrapProgram





goodProgram :: CProgram 'UnitTy
goodProgram = mkProg $ do
    let a = 0
    let b = 1
    let c = 2

    allocate a
    a |= Cst 0
    free a


goodProgram1 :: CProgram 'UnitTy
goodProgram1 = mkProg $ do
    let a = 0
    let b = 1
    let c = 2

    allocate b 
    b |= Cst 23 

    allocate c 
    c |= Cst 24 

    allocate a
    a |= BinOp Add 
            (Var b)
            (Var c)

    free a
    free b
    free c




badProgram :: CProgram 'UnitTy
badProgram = mkProg $ do
    let a = 0
    let b = 1
    let c = 2

    allocate a
    a |= Cst 0
    -- forgetting to free



badProgram1 :: CProgram 'UnitTy
badProgram1 = mkProg $ do
    let a = 0
    let b = 1
    let c = 2

    allocate a
    allocate b
    -- giving a an undefined value
    a |= Var b
    free a
    free b




badProgram2 :: CProgram 'UnitTy
badProgram2 = mkProg $ do
    let a = 0
    let b = 1
    let c = 2

    allocate a
    a |= Cst 0
    free a

    -- Using a after it has been freed
    allocate b
    b |= Cst 0
    b |= Var a
    free b


