module Test.Pretty where

import Test.Tasty
import Test.Tasty.HUnit

import IR.Syntax
import IR.Program
import Pretty


allTests :: TestTree
allTests = testGroup 
                "Pretty printing"
                [ instrTest 
                , moduleTest
                , exprTest ]



exprTest :: TestTree
exprTest = testCase "Test pretty printing expressions" $ do
    let expr = (Cst 0) .+. (Deref $ Var 1)
    prettyShow expr @?= "0 + *$1"

    let expr = (Cst 0) .+. (Deref $ (Var 1) `Offset` (Cst 12))
    prettyShow expr @?= "0 + *($1 + 12)"

    let expr = (Deref $ Allocate $ Cst 12) .+. (Deref $ Var 1)
    prettyShow expr @?= "*allocate(12) + *$1"

instrTest :: TestTree
instrTest = testCase "Test pretty printing instructions" $ do
    let instr = Set (Var 0) (Cst 34) :: IRInstr String
    prettyShow instr @?= "$0 *= 34"

moduleTest :: TestTree
moduleTest = testCase "Test pretty printing modules" $ do
    let mainModule = mkProg $ do
              "something" ~> do
                    a <- allocate1_
                    free $ Var a
              "otherwise" ~> do
                    b <- allocate1_
                    Var b *= (Deref $ Var b) .+. (Cst 34)

    prettyShow mainModule @?= "[\"otherwise\"]:\n\t$1 := allocate(1)\n\t$1 *= *$1 + 34\n\n[\"something\"]:\n\t$0 := allocate(1)\n\tfree $0\n\n"
