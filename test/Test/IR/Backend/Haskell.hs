module Test.IR.Backend.Haskell where

import Test.Tasty
import Test.Tasty.HUnit

import IR.Syntax
import IR.Program
import IR.Backend.Haskell
import IR.Backend.Haskell.State


allTests :: TestTree
allTests = testGroup 
                "Haskell Runtime Backend"
                [ goodProgramTest 
                , spaceLeaks 
                , canReallocate 
                , outOfRange 
                , usingUnassignedValuesTest ]

goodProgramTest :: TestTree
goodProgramTest = testCase "Good programs" $ do

    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_
            a .= Cst 32
            free a

    compileAndRun program @?= Nothing

    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 4
            (Var a) `Offset` (Cst 3) *= Cst 32
            free a

    compileAndRun program @?= Nothing


    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            b .= Var a
            a .= Cst 32
            Deref (Var b) *= Cst 33
            free a
            free b

    compileAndRun program @?= Nothing

 
usingUnassignedValuesTest :: TestTree
usingUnassignedValuesTest = testCase "Using unassigned values" $ do
    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            a .= Deref (Var b) `asType` intTy
            free a
            free b

    stripLoc (compileAndRun program) @?= 
        Just (UsingUnassignedValue (IRAddrOffset 1 0))

    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            a .= Cst 32
            Deref (Var b) *= Cst 33
            free a
            free b

    stripLoc (compileAndRun program) @?= 
        Just (UsingUnassignedValue (IRAddrOffset 1 0))


    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 2
            b <- allocate1_ 
            a .= Cst 32
            b .= Deref ((Var a) `Offset` (Cst 1)) `asType` intTy
            free a
            free b

    stripLoc (compileAndRun program) @?= 
        Just (UsingUnassignedValue (IRAddrOffset 0 1))

spaceLeaks :: TestTree
spaceLeaks = testCase "Using unassigned values" $ do
    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            a .= Cst 3
            b .= Var a

    stripLoc (compileAndRun program) @?= 
        Just (MemoryLeak [0, 1])




canReallocate :: TestTree
canReallocate = testCase "Reallocation" $ do
    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            a .= Cst 3
            free a
            allocate1 a
            a .= Cst 4
            free a

    stripLoc (compileAndRun program) @?= Nothing

    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            a .= Cst 3
            -- free a -- forget to free
            allocate1 a
            a .= Cst 4
            free a

    stripLoc (compileAndRun program) @?= Just (MemoryLeak [0])



outOfRange :: TestTree
outOfRange = testCase "Out of range" $ do
    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 24 
            (Var a) `Offset` (Cst 23) *= Cst 2
            free a

    stripLoc (compileAndRun program) @?= Nothing

    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 24 
            (Var a) `Offset` (Cst 24) *= Cst 2
            free a

    stripLoc (compileAndRun program) @?= Just (OutOfRange (IRAddrOffset (IRAddr 0) 24) 24)


