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
             free (Var x)
    let expected :: Module String
        expected = oneScope $
                        [ Is 0 (Allocate (Cst 3))
                        , Set (Var 0) (Cst 3)
                        , Set (Offset (Var 0) (Cst 1)) (Cst 5)
                        , Free (Var 0) ]

    program @?= expected

    let program :: Module String
        program = mkProg $ do
             x <- allocate1_
             y <- allocate1_
             x .= Cst 3 
             y .= Deref (Var x) .+. (Cst 1)
             free (Var x)
             free (Var y)
    let expected :: Module String
        expected = oneScope $
                        [ Is 0 (Allocate (Cst 1))
                        , Is 1 (Allocate (Cst 1))
                        , Set (Var 0) (Cst 3)
                        , Set (Var 1) (BinOp Add (Deref (Var 0)) (Cst 1))
                        , Free (Var 0)
                        , Free (Var 1)] 
                        
    program @?= expected

multiLabelModule :: TestTree
multiLabelModule = testCase "Defining multilabel module monadically" $ do
    let program :: Module String
        program = mkProg $ do
             x <- allocate1_
             jump "a"

             "a" ~> do
                _ <- allocate1_
                free (Var 2)

             "b" ~> do
                x .= Cst 34

             free (Var x)

    let expected :: Module String
        expected = Map.fromList $
                        [ (def, [ Loc (IRLoc def 0) (Is 0 (Allocate (Cst 1)))
                                , Loc (IRLoc def 1) (JComp Eq (Cst 0) (Cst 0) "a")
                                , Loc (IRLoc def 2) (Free (Var 0))])

                        , ("a", [ Loc (IRLoc "a" 0) (Is 1 (Allocate (Cst 1)))
                                , Loc (IRLoc "a" 1) (Free (Var 2))])
                        , ("b", [ Loc (IRLoc "b" 0) (Set (Var 0) (Cst 34))]) ]
    program @?= expected