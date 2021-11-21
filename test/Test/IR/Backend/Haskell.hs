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
                , allocatingAcrossJumps
                , multiLabelTest
                , memoryLeakTest           ]

useAfterFreeTest :: TestTree
useAfterFreeTest = testCase "UseAfterFree" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate a
                allocate b
                a |= Cst 32
                free a
                b |= BinOp Add (Cst 21) (Var a)
                free b

        compileAndRun program @?= Just (UseAfterFree a)


usingUnassignedValueTest :: TestTree
usingUnassignedValueTest = testCase "UsingUnassignedValue" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate a
                allocate b
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b

        compileAndRun program @?= Just (UsingUnassignedValue a)


memoryLeakTest :: TestTree
memoryLeakTest = testCase "MemoryLeak" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate a
                allocate b
                allocate c
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                c |= BinOp Add (Var b)  (Var a)
                free a
                free b
                -- free c

        compileAndRun program @?= Just (MemoryLeak [c])

redeclarationTest :: TestTree
redeclarationTest = testCase "Redeclaration" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate a
                allocate b
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)

                allocate a
                a |= Cst 32

                free a
                free b
                -- free c

        compileAndRun program @?= Just (Redeclaration a)

deallocatingUnallocatedTest :: TestTree
deallocatingUnallocatedTest = testCase "DeallocatingUnallocated" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate a
                allocate b
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b
                free a
                -- free c

        compileAndRun program @?= Just (DeallocatingUnallocated a)

assigningUnallocatedTest :: TestTree
assigningUnallocatedTest = testCase "AssigningUndeclared" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate b
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b

        compileAndRun program @?= Just (AssigningUndeclared a)

allocatingAcrossJumps :: TestTree
allocatingAcrossJumps = testCase "Allocating across jumps : good program" $ do
         let a:b:c:_ = [0..]
         let program = mkProg $ do
                         allocate a
                         jump "test"

                         "test" ~> do
                              a |= Cst 23
                              free a
         compileAndRun program @?= Nothing

         let program = mkProg $ do
                         allocate a
                         jump "test"

                         "test" ~> do
                              a |= Cst 23
                              jeq (Var a) (Cst 23) "free"

                         "free" ~> do
                              free a
 
         compileAndRun program @?= Nothing

goodProgramTest :: TestTree
goodProgramTest = testCase "Good program 1" $ do
        let program :: Module Int
            program = mkProg $ do
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

        compileAndRun_ (Map.unionWith Seq program freeProgram) @?= (Nothing, Map.empty)
        let registerAfterProgram = snd $ compileAndRun_ program
        registerAfterProgram @?= Map.fromList  [ (a, Just 92) , (b, Just 34) , (c, Just 24)]

{-
Program test if "number" is odd

-}
multiLabelTest :: TestTree
multiLabelTest = testCase "Good program 2: is odd?" $ do
         let number:to_return:_ = [0..]
         let program :: Module String
             program = mkProg $ do
               allocate number
               allocate to_return
               number |= Cst 3
               jump "subBy2"

               "subBy2" ~> do 
                  number |= BinOp Sub
                              (Var number)
                              (Cst 2)
                  jeq (Var number) (Cst 0) "end"
                  jeq (Var number) (Cst 1) "end"
                  jump "subBy2"
                  jump "end"

               "end" ~> do
                  to_return |= Var number
                  jump "freeVars"

             freeVars :: Module String
             freeVars = mkProg $ "freeVars" ~> do 
                                    free number
                                    free to_return

         compileAndRun (Map.union program freeVars) @?= Nothing
         let (_, register) = compileAndRun_ program
         register Map.! to_return @?= Just 1





------------------- UTILS -----------------

a, b, c :: CName
a = 1
b = 2
c = 3