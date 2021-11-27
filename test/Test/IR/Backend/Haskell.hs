module Test.IR.Backend.Haskell where

import Control.Lens hiding ((.=), (*=))
import Data.Vector ((!?))
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
                , complexProgram 
                , usingUnassignedValuesTest ]

goodProgramTest :: TestTree
goodProgramTest = testCase "Good programs" $ do

    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_
            a .= Cst 32
            free (Var a)

    compileAndRun program @?= Nothing

    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 4
            (Var a) `Offset` (Cst 3) *= Cst 32
            free (Var a)

    compileAndRun program @?= Nothing


    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            b .= Var a
            a .= Cst 32
            Deref (Var b) *= Cst 33
            free (Var a)
            free (Var b)

    compileAndRun program @?= Nothing

 
usingUnassignedValuesTest :: TestTree
usingUnassignedValuesTest = testCase "Using unassigned values" $ do
    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            a .= Deref (Var b) `asType` intTy
            free (Var a)
            free (Var b)

    stripLoc (compileAndRun program) @?= 
        Just (UsingUnassignedValue (IRAddrOffset 1 0))

    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            b <- allocate1_ 
            a .= Cst 32
            Deref (Var b) *= Cst 33
            free (Var a)
            free (Var b)

    stripLoc (compileAndRun program) @?= 
        Just (UsingUnassignedValue (IRAddrOffset 1 0))


    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 2
            b <- allocate1_ 
            a .= Cst 32
            b .= Deref ((Var a) `Offset` (Cst 1)) `asType` intTy
            free (Var a)
            free (Var b)

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
            free (Var a)
            allocate1 a
            a .= Cst 4
            free (Var a)

    stripLoc (compileAndRun program) @?= Nothing

    let program :: Module Int
        program = mkProg $ do
            a <- allocate1_ 
            a .= Cst 3
            -- free (Var a) -- forget to free
            allocate1 a
            a .= Cst 4
            free (Var a)

    stripLoc (compileAndRun program) @?= Just (MemoryLeak [0])



outOfRange :: TestTree
outOfRange = testCase "Out of range" $ do
    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 24 
            (Var a) `Offset` (Cst 23) *= Cst 2
            free (Var a)

    stripLoc (compileAndRun program) @?= Nothing

    let program :: Module Int
        program = mkProg $ do
            a <- allocateN_ 24 
            (Var a) `Offset` (Cst 24) *= Cst 2
            free (Var a)

    stripLoc (compileAndRun program) @?= Just (OutOfRange (IRAddrOffset (IRAddr 0) 24) 24)

complexProgram :: TestTree
complexProgram = testCase "Complex program" $ do
    let fibonacci :: Module String
        fibonacci = mkProg $ do
            n <- allocate1_
            n .= Cst 12

            results <- allocate_ $ Deref (Var n)

            (Var results) `Offset` (Cst 0) *= Cst 1
            (Var results) `Offset` (Cst 1) *= Cst 1

            current <- allocate1_
            current .= Cst 2

            jump "computeCurrent"

            "computeCurrent" ~> do
                let prev  = Deref $ (Var results) `Offset` ((*.) current .-. Cst 1)
                let pprev = Deref $ (Var results) `Offset` ((*.) current .-. Cst 2)
                (Var results) `Offset` ((*.) current) *= prev .+. pprev
                current .= (*.) current .+. (Cst 1)
                jeq  ((*.) current) ((*.) n) "end"
                jneq ((*.) current) ((*.) n) "computeCurrent"

            "end" ~> do
                free (Var n)

    let finalState = snd (compileAndRun_ fibonacci)
        value = do 
            vec      <- finalState ^. register . (at 1)
            maybeVal <- vec !? 11
            val      <- maybeVal
            preview _IntVal val

    value @?= Just 144 -- fib 11 = 144



