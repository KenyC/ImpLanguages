module Test.IR.Syntax where

import Data.Default
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import IR.Syntax
import IR.Program


allTests :: TestTree
allTests = testGroup 
                "Syntax"
                [ oneScopeModule 
                , multiLabelModule ]


oneScopeModule :: TestTree
oneScopeModule = testCase "One scope program" $ do
    let addLoc = zipWith Loc $ map (IRLoc def) [0..]
    let oneScope x = Map.fromList [(def, addLoc x)]

    let program :: Module String
        program = mkProg $ do
             x <- allocate_ $ Cst 3
             x .= Cst 3 
             (Var x) `Offset` (Cst 1) *= Cst 5
             free x
    let expected :: Module String
        expected = oneScope $
                        [ Allocate 0 (Cst 3)
                        , Set (Var 0) (Cst 3)
                        , Set (Offset (Var 0) (Cst 1)) (Cst 5)
                        , Free 0 ]

    program @?= expected

    let program :: Module String
        program = mkProg $ do
             x <- allocate1_
             y <- allocate1_
             x .= Cst 3 
             y .= Deref (Var x) .+. (Cst 1)
             free x
             free y
    let expected :: Module String
        expected = oneScope $
                        [ Allocate 0 (Cst 1)
                        , Allocate 1 (Cst 1)
                        , Set (Var 0) (Cst 3)
                        , Set (Var 1) (BinOp Add (Deref (Var 0)) (Cst 1))
                        , Free 0
                        , Free 1] 
                        
    program @?= expected

multiLabelModule :: TestTree
multiLabelModule = testCase "Defining multilabel module monadically" $ do
    let program :: Module String
        program = mkProg $ do
             x <- allocate1_
             jump "a"

             "a" ~> do
                _ <- allocate1_
                free 2

             "b" ~> do
                x .= Cst 34

             free x

    let expected :: Module String
        expected = Map.fromList $
                        [ (def, [Loc (IRLoc def 0) (Allocate 0 (Cst 1)), Loc (IRLoc def 1) (JComp Eq (Cst 0) (Cst 0) "a"), Loc (IRLoc def 2) (Free 0)])
                        , ("a", [Loc (IRLoc "a" 0) (Allocate 1 (Cst 1)), Loc (IRLoc "a" 1) (Free 2)])
                        , ("b", [Loc (IRLoc "b" 0) (Set (Var 0) (Cst 34))]) ]
    program @?= expected