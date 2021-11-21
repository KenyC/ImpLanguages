module Test.IR.Backend.Haskell where

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import IR.Syntax
import IR.Backend.Haskell


allTests :: TestTree
allTests = testGroup 
                "Haskell Backend"
                [ usingUnassignedValueTest 
                , redeclarationTest
                , deallocatingUnallocatedTest           
                , assigningUnallocatedTest
                , useAfterFreeTest
                , goodProgramTest
                , memoryLeakTest           ]

useAfterFreeTest :: TestTree
useAfterFreeTest = testCaseSteps "UseAfterFree" $ \step -> do
        let program = mkProg $ do
                allocate a
                allocate b
                a |= Cst 32
                free a
                b |= BinOp Add (Cst 21) (Var a)
                free b

        compileAndRun program @?= Left UseAfterFree


usingUnassignedValueTest :: TestTree
usingUnassignedValueTest = testCaseSteps "UsingUnassignedValue" $ \step -> do
        let program = mkProg $ do
                allocate a
                allocate b
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b

        compileAndRun program @?= Left UsingUnassignedValue


memoryLeakTest :: TestTree
memoryLeakTest = testCaseSteps "MemoryLeak" $ \step -> do
        let program = mkProg $ do
                allocate a
                allocate b
                allocate c
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                c |= BinOp Add (Var b)  (Var a)
                free a
                free b
                -- free c

        compileAndRun program @?= Left MemoryLeak

redeclarationTest :: TestTree
redeclarationTest = testCaseSteps "Redeclaration" $ \step -> do
        let program = mkProg $ do
                allocate a
                allocate b
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)

                allocate a
                a |= Cst 32

                free a
                free b
                -- free c

        compileAndRun program @?= Left Redeclaration

deallocatingUnallocatedTest :: TestTree
deallocatingUnallocatedTest = testCaseSteps "DeallocatingUnallocated" $ \step -> do
        let program = mkProg $ do
                allocate a
                allocate b
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b
                free a
                -- free c

        compileAndRun program @?= Left DeallocatingUnallocated

assigningUnallocatedTest :: TestTree
assigningUnallocatedTest = testCaseSteps "AssigningUndeclared" $ \step -> do
        let program = mkProg $ do
                allocate b
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b

        compileAndRun program @?= Left AssigningUndeclared

goodProgramTest :: TestTree
goodProgramTest = testCaseSteps "Good program 1" $ \step -> do
        let program = mkProg $ do
                allocate b
                b |= Cst 34

                allocate c
                c |= Cst 24

                allocate a
                a |= BinOp Add (Var b) (Var c)
                a |= BinOp Add (Var a) (Var b)

        let freeProgram = mkProg $ do
                free a
                free b
                free c

        compileAndRun_ (Seq [program, freeProgram]) @?= (Right (), Map.empty)
        compileAndRun_ program @?= ( Left MemoryLeak
                                   , Map.fromList $ 
                                        [ (a, Just 92)
                                        , (b, Just 34)
                                        , (c, Just 24)])

------------------- UTILS -----------------

a, b, c :: CName
a = 1
b = 2
c = 3