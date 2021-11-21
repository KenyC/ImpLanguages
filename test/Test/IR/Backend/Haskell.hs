module Test.IR.Backend.Haskell where

import Control.Lens
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import IR.Syntax
import IR.Backend.Haskell


allTests :: TestTree
allTests = testGroup 
                "Haskell Backend"
                [ goodProgram2Test 
                , usingUnassignedValueTest 
                , redeclarationTest
                , deallocatingUnallocatedTest           
                , assigningUnallocatedTest
                , useAfterFreeTest
                , goodProgramTest
                , loggerTest
                , allocatingAcrossJumps
                , multiLabelTest
                , memoryLeakTest           ]

useAfterFreeTest :: TestTree
useAfterFreeTest = testCase "UseAfterFree" $ do
        let program :: Module Int
            program = mkProg $ do
                a <- allocate_
                b <- allocate_
                a |= Cst 32
                free a
                b |= BinOp Add (Cst 21) (Var a)
                free b

            a = 0
        compileAndRun program @?= Just (UseUnallocated a)


usingUnassignedValueTest :: TestTree
usingUnassignedValueTest = testCase "UsingUnassignedValue" $ do
        let program :: Module Int
            program = mkProg $ do
                a <- allocate_
                b <- allocate_
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b
            a = 0
        compileAndRun program @?= Just (UsingUnassignedValue a)


memoryLeakTest :: TestTree
memoryLeakTest = testCase "MemoryLeak" $ do
        let program :: Module Int
            program = mkProg $ do
                a <- allocate_
                b <- allocate_
                c <- allocate_
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                c |= BinOp Add (Var b)  (Var a)
                free a
                free b
                -- free c
            c = 2
        compileAndRun program @?= Just (MemoryLeak [c])

redeclarationTest :: TestTree
redeclarationTest = testCase "Redeclaration" $ do
        let program :: Module Int
            program = mkProg $ do
                a <- allocate_
                b <- allocate_

                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)

                allocate a
                a |= Cst 32

                free a
                free b
                -- free c
            addr = 0
        compileAndRun program @?= Just (MemoryLeak [addr])

deallocatingUnallocatedTest :: TestTree
deallocatingUnallocatedTest = testCase "DeallocatingUnallocated" $ do
        let program :: Module Int
            program = mkProg $ do
                a <- allocate_
                b <- allocate_
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b
                free a
                -- free c
            a = 0
        compileAndRun program @?= Just (DeallocatingUnallocated a)

assigningUnallocatedTest :: TestTree
assigningUnallocatedTest = testCase "AssigningUnallocated" $ do
        let program :: Module Int
            program = mkProg $ do
                a <- newName
                b <- allocate_ 
                a |= Cst 34
                b |= BinOp Add (Cst 21) (Var a)
                free a
                free b
            a = 0
        compileAndRun program @?= Just (AssigningUnallocated a)

allocatingAcrossJumps :: TestTree
allocatingAcrossJumps = testCase "Allocating across jumps : good program" $ do
         let program = mkProg $ do
                         a <- allocate_
                         jump "test"

                         "test" ~> do
                              a |= Cst 23
                              free a
         compileAndRun program @?= Nothing

         let program = mkProg $ do
                         a <- allocate_
                         jump "test"

                         "test" ~> do
                              a |= Cst 23
                              jeq (Var a) (Cst 23) "free"

                         "free" ~> do
                              free a
 
         compileAndRun program @?= Nothing

goodProgramTest :: TestTree
goodProgramTest = testCase "Good program 1" $ do
        let a:b:c:_ = [0..]
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

        let testError = fst $ compileAndRun_ (Map.unionWith (++) program freeProgram) 
        testError @?= Nothing
        let registerAfterProgram = _register $ snd $ compileAndRun_ program
        registerAfterProgram @?= Map.fromList  [ (0, Just 34) , (1, Just 24) , (2, Just 92)]

goodProgram2Test :: TestTree
goodProgram2Test = testCase "Good program 2 : simplest" $ do
        let program :: Module Int
            program = mkProg $ do
                allocate 0
                free     0
        let testError = fst $ compileAndRun_ program
        testError @?= Nothing

loggerTest :: TestTree
loggerTest = testCase "test logger" $ do
    let simpleProgram :: Module String
        simpleProgram = mkProg $ do
            a <- allocate_
            a |= Cst 0
            free a

    let logResult = view (_2 . logger) $ compileAndRun_ simpleProgram
    let expectedLog = [ "free CName 0"
                      , "CName 0 <- Cst (CIntVal 0) (= CIntVal 0)"
                      , "allocate CName 0"]

    logResult @?= expectedLog

{-
Program test if "number" is odd

-}
multiLabelTest :: TestTree
multiLabelTest = testCase "Good program 2: is odd?" $ do

         let number:to_return:_ = [0..]
         let freeVars :: Module String
             freeVars = mkProg $ do
                            "end"      ~> jump "freeVars"
                            "freeVars" ~> do 
                                free number
                                free to_return

         compileAndRun (Map.unionWith (++) freeVars multiLabelProgram) @?= Nothing
         let results  = snd $ compileAndRun_ multiLabelProgram
         let register = _register results
         let nameMap  = _nameMap  results
         -- liftIO $ forM_ (reverse $ _logger results) putStrLn
         -- liftIO $  putStrLn "############"
         register Map.! (nameMap Map.! to_return) @?= Just 1


multiLabelProgram :: Module String
multiLabelProgram = mkProg $ do
  let number:to_return:_ = [0..]
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
     jump "end"
     -- jump "subBy2"

  "end" ~> do
     to_return |= Var number



------------------- UTILS -----------------

