{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language where

import Control.Lens
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Machine
import Types

import Debug.Trace

debug x = traceShow x x 

{-
let x = 1
let y = 2
let z = x + y

if z == 0
    x = x + 1
else
    x = x - 1

-}

data ExprTy 
    = Value
    | Unit
    deriving (Eq, Show)


data Syntax name exprTy where
    Pass :: Syntax name 'Unit
    Let ::
           name 
        -> Syntax name 'Value 
        -> Syntax name 'Unit

    Assign ::
           name 
        -> Syntax name 'Value 
        -> Syntax name 'Unit

    Cst :: Val  -> Syntax name 'Value
    Var :: name -> Syntax name 'Value
    (:+:) :: Syntax name 'Value -> Syntax name 'Value -> Syntax name 'Value
    (:-:) :: Syntax name 'Value -> Syntax name 'Value -> Syntax name 'Value

    IfEq :: 
         Syntax name 'Value 
      -> Syntax name 'Value 
      -> Syntax name 'Unit
      -> Syntax name 'Unit

    IfNeq :: 
         Syntax name 'Value 
      -> Syntax name 'Value 
      -> Syntax name 'Unit
      -> Syntax name 'Unit

    Seq :: 
        [Syntax name 'Unit] 
      -> Syntax name 'Unit

deriving instance (Show name) => Show (Syntax name exprTy)

instance Semigroup (Syntax name 'Unit) where
    (<>) (Seq l) (Seq l') = Seq $ l ++ l' 
    (<>) (Seq l) x        = Seq $ l ++ [x]
    (<>) x       (Seq l') = Seq $ x:l'
    (<>) x       y        = Seq $ [x, y]

instance Monoid (Syntax name 'Unit) where
    mempty = Pass

{-
let x = 1
let y = 2
let z = x + y

if z == 0
    x = x + 1
else
    x = x - 1

-}

program1 :: Syntax String 'Unit 
program1 = mconcat
    [ Let "x" $ Cst 1
    , Let "y" $ Cst 2
    , Let "z" $ (Var "x") :+: (Var "y")
    , IfEq (Var "z") (Cst 0) $
        Assign "x" $ (Var "x") :+: (Cst 1)
    , IfNeq (Var "z") (Cst 0) $
        Assign "x" $ (Var "x") :-: (Cst 1)
    , Pass ]

badProgram1 :: Syntax String 'Unit 
badProgram1 = mconcat
    [ Let "x" $ Cst 1
    , Let "y" $ Cst 2
    , Let "z" $ (Var "x") :+: (Var "y")
    , IfEq (Var "z") (Cst 0) $
        Assign "e" $ (Var "x") :+: (Cst 1)
    , IfNeq (Var "z") (Cst 0) $
        Assign "e" $ (Var "x") :-: (Cst 1)
    , Pass ]

badProgram2 :: Syntax String 'Unit 
badProgram2 = mconcat
    [ Let "x" $ Cst 1
    , Let "y" $ Cst 2
    , Let "z" $ (Var "x") :+: (Var "y")
    , Let "x" $ Cst 3
    , IfEq (Var "z") (Cst 0) $
        Assign "z" $ (Var "x") :+: (Cst 1)
    , IfNeq (Var "z") (Cst 0) $
        Assign "z" $ (Var "x") :-: (Cst 1)
    , Pass ]


data CompileError
    = UnknownName
    | ReassigningName
    deriving (Eq, Show)

data CompileState name = CompileState {
    _names :: Map name Addr,
    _freeStack :: [[Machine ()]]
}
makeLenses ''CompileState

initialState :: CompileState name
initialState = CompileState {
      _names     = Map.empty
    , _freeStack = [[]]
}

newtype Compiler name a = Compiler {
    unwrapCompiler :: 
        ExceptT CompileError 
        (StateT (CompileState name) a)
} 
deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState CompileState
    , MonadError CompileError)

compile :: (Ord name) => Syntax name 'Unit -> Compiler name (Machine ())
compile Pass = return (return ())
compile (Let varName expr) = do
    programValue <- evaluate expr
    return $ 


evaluate :: (Ord name) => Syntax name 'Value -> Compiler name (Machine Val)
evaluate = undefined

-- nameCheck :: (Ord name, Show name) => Syntax name 'Unit -> Bool
nameCheck :: (Ord name) => Syntax name 'Unit -> Bool
nameCheck =  getAll . fst . (nameCheck' Set.empty) where 
    nameCheck' :: 
           -- (Ord name, Show name)
           (Ord name)
        => Set name
        -> Syntax name exprTy
        -> (All, Set name)
    nameCheck' knownNames (Let var value) = 
        -- traceShow knownNames $ 
        ( All $ var `Set.notMember` knownNames, Set.singleton var) <>
        (nameCheck' knownNames value)

    nameCheck' knownNames (Assign var value) =
        ( All $ var `Set.member` knownNames, Set.empty) <>
        (nameCheck' knownNames value)

    nameCheck' knownNames (Cst _) =
        (All True, Set.empty)

    nameCheck' knownNames (Var var) =
        (All $ var `Set.member` knownNames, Set.empty)

    nameCheck' knownNames ((:+:) val1 val2) =
        nameCheck' knownNames val1 <>
        nameCheck' knownNames val2

    nameCheck' knownNames ((:-:) val1 val2) =
        nameCheck' knownNames val1 <>
        nameCheck' knownNames val2

    nameCheck' knownNames (Seq [])    = (All True, Set.empty)
    nameCheck' knownNames (Seq (x:l)) =  
        case nameCheck' knownNames x of
            (All True, newNames) -> nameCheck' (knownNames <> newNames) (Seq l) 
            x -> x

    nameCheck' knownNames (IfEq val1 val2 scope) = 
        set _2 knownNames $ 
            mconcat 
                [ nameCheck' knownNames val1
                , nameCheck' knownNames val2
                , nameCheck' knownNames scope ]


    nameCheck' knownNames (IfNeq val1 val2 scope) = 
        set _2 knownNames $ 
            mconcat 
                [ nameCheck' knownNames val1
                , nameCheck' knownNames val2
                , nameCheck' knownNames scope ]

    nameCheck' knownNames Pass = (mempty, knownNames) 

