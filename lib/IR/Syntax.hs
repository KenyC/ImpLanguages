module IR.Syntax where

import Control.Lens
import Control.Monad.State.Strict
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf

{-
Language spec

int* val = allocate; // allocate val

*val = 23         // setVal 

skipneq *val

free(val);
-}

-- SYNTAX

data CType
    = UnitTy
    | IntTy
    | AddrTy
    deriving (Eq, Show)

type family IsExpr a where
    IsExpr UnitTy = 'False
    IsExpr a      = 'True



data CBinOp
    = Add
    | Sub
    deriving (Eq, Show)
toHaskOp Add = (+)
toHaskOp Sub = (-)

data CCompOp
    = Eq
    | NEq
    | More
    | MoreEq
    deriving (Eq, Show)
toHaskCompOp Eq     = (==) 
toHaskCompOp NEq    = (/=) 
toHaskCompOp More   = (>) 
toHaskCompOp MoreEq = (>=) 

newtype CName   = CName   Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 
newtype CIntVal = CIntVal Int deriving (Num, Eq, Show, Enum, Ord, PrintfArg) 



data CScope scopeLabel exprTy where
    -- Allocation
    Allocate :: CName -> Int -> CScope scopeLabel 'UnitTy
    Free     :: CName -> CScope scopeLabel 'UnitTy

    -- Expressions
    Deref ::
         (IsExpr a ~ 'True)
      => CScope scopeLabel 'AddrTy
      -> CScope scopeLabel a

    Offset ::
         CScope scopeLabel 'AddrTy
      -> Int
      -> CScope scopeLabel 'AddrTy


    Cst :: 
         CIntVal
      -> CScope scopeLabel 'IntTy

    Var :: 
         CName
      -> CScope scopeLabel 'IntTy

    BinOp :: 
         CBinOp
      -> CScope scopeLabel 'IntTy
      -> CScope scopeLabel 'IntTy
      -> CScope scopeLabel 'IntTy

    -- Set
    Set ::
        CName
     -> CScope scopeLabel 'IntTy
     -> CScope scopeLabel 'UnitTy

    -- Control structures
    -- Seq ::
    --       CScope scopeLabel 'UnitTy
    --   ->  CScope scopeLabel 'UnitTy
    --   ->  CScope scopeLabel 'UnitTy
    Pass :: CScope scopeLabel 'UnitTy

    -- 
    Jump ::
         scopeLabel
      -> CScope scopeLabel 'UnitTy

    JComp ::
         CCompOp
      -> CScope scopeLabel 'IntTy
      -> CScope scopeLabel 'IntTy
      -> scopeLabel
      -> CScope scopeLabel 'UnitTy


deriving instance (Eq scopeLabel)   => Eq   (CScope scopeLabel a)
deriving instance (Show scopeLabel) => Show (CScope scopeLabel a)

-- instance Semigroup (CScope scopeLabel 'UnitTy) where
--     (<>) = Seq 

type Module scopeLabel = Map scopeLabel [CScope scopeLabel 'UnitTy]

data ProgramState scopeLabel = ProgramState {
    _moduleProg   :: Module scopeLabel,
    _currentLabel :: scopeLabel,
    _nextName     :: CName
}
makeLenses ''ProgramState

initialProgramState :: (Default scopeLabel) => ProgramState scopeLabel
initialProgramState = ProgramState {
    _moduleProg   = Map.empty,
    _currentLabel = def,
    _nextName     = 0
}


newtype CProgram scopeLabel a = CProgram {
    unwrapProgram :: State (ProgramState scopeLabel) a
} deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (ProgramState scopeLabel) )

------------------- UTILS -----------------

addToLabel 
    :: (Ord scopeLabel) 
    => CScope scopeLabel 'UnitTy 
    -> CProgram scopeLabel ()
addToLabel instruction = do
    label <- use currentLabel
    modifying moduleProg $ Map.insertWith (flip (++)) label [instruction]

--
-- Allocation
allocate :: (Ord scopeLabel) => CName -> Int -> CProgram scopeLabel ()
allocate name n = addToLabel $ Allocate name n 

allocate_ :: (Ord scopeLabel) => Int -> CProgram scopeLabel CName
allocate_ n = do
    name <- newName
    allocate name n
    return name

newName :: CProgram scopeLabel CName
newName = do 
    name <- use nextName
    modifying nextName (+1)
    return name

free :: (Ord scopeLabel) => CName -> CProgram scopeLabel ()
free name = addToLabel $ Free name 

jump :: (Ord scopeLabel) => scopeLabel -> CProgram scopeLabel ()
jump label = addToLabel $ Jump label 

jcomp :: 
    (Ord scopeLabel) 
 => CCompOp 
 -> CScope scopeLabel 'IntTy 
 -> CScope scopeLabel 'IntTy 
 -> scopeLabel 
 -> CProgram scopeLabel ()
jcomp op expr1 expr2 label = addToLabel $ JComp op expr1 expr2 label  


jeq :: 
    (Ord scopeLabel) 
 => CScope scopeLabel 'IntTy 
 -> CScope scopeLabel 'IntTy 
 -> scopeLabel 
 -> CProgram scopeLabel ()
jeq = jcomp Eq  

jneq :: 
    (Ord scopeLabel) 
 => CScope scopeLabel 'IntTy 
 -> CScope scopeLabel 'IntTy 
 -> scopeLabel 
 -> CProgram scopeLabel ()
jneq = jcomp NEq  


(|=) 
    :: (Ord scopeLabel)
    => CName
    -> CScope scopeLabel 'IntTy
    -> CProgram scopeLabel ()
(|=) name expr = addToLabel $ Set name expr

(~>) :: scopeLabel -> CProgram scopeLabel () -> CProgram scopeLabel ()
(~>) label scope = do 
    oldLabel <- use currentLabel
    assign currentLabel label
    scope
    assign currentLabel oldLabel





mkProg :: (Default scopeLabel) => CProgram scopeLabel a -> Module scopeLabel
mkProg = 
    view moduleProg .
    (flip execState initialProgramState) . 
    unwrapProgram


