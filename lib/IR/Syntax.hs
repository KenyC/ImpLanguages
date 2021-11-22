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
data CExprTy
    = IntTy
    | AddrTy
    deriving (Eq, Show)

data CType
    = UnitTy
    | E CExprTy
    deriving (Eq, Show)





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
         CScope scopeLabel (E 'AddrTy)
      -> CScope scopeLabel (E (a :: CExprTy))

    Offset ::
         CScope scopeLabel (E 'AddrTy)
      -> Int
      -> CScope scopeLabel (E 'AddrTy)


    Cst :: 
         CIntVal
      -> CScope scopeLabel (E 'IntTy)

    Var :: 
         CName
      -> CScope scopeLabel (E 'IntTy)

    BinOp :: 
         CBinOp
      -> CScope scopeLabel (E 'IntTy)
      -> CScope scopeLabel (E 'IntTy)
      -> CScope scopeLabel (E 'IntTy)

    -- Set
    Set ::
        CName
     -> CScope scopeLabel (E 'IntTy)
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
      -> CScope scopeLabel (E 'IntTy)
      -> CScope scopeLabel (E 'IntTy)
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
 -> CScope scopeLabel (E 'IntTy) 
 -> CScope scopeLabel (E 'IntTy) 
 -> scopeLabel 
 -> CProgram scopeLabel ()
jcomp op expr1 expr2 label = addToLabel $ JComp op expr1 expr2 label  


jeq :: 
    (Ord scopeLabel) 
 => CScope scopeLabel (E 'IntTy) 
 -> CScope scopeLabel (E 'IntTy) 
 -> scopeLabel 
 -> CProgram scopeLabel ()
jeq = jcomp Eq  

jneq :: 
    (Ord scopeLabel) 
 => CScope scopeLabel (E 'IntTy) 
 -> CScope scopeLabel (E 'IntTy) 
 -> scopeLabel 
 -> CProgram scopeLabel ()
jneq = jcomp NEq  


(|=) 
    :: (Ord scopeLabel)
    => CName
    -> CScope scopeLabel (E 'IntTy)
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


