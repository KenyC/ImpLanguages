module Test.IR.Backend.Asm where

import Control.Monad.IO.Class
import Control.Lens hiding ((.=), (*=))
import Data.Vector ((!?))
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified CodeGen.X86 as X86

import IR.Syntax
import IR.Program
import IR.Backend.Asm
import IR.Backend.Asm.RunCode


allTests :: TestTree
allTests = testGroup 
                "Assembly Runtime Backend"
                [ simpleExprTest ]

simpleExprTest :: TestTree
simpleExprTest = testCase "Simple constant expr" $ do
    let expr = Cst 1
    val <- liftIO $ (X86.compile (compileExprInt expr) :: IO Word64) 
    val @?= 1 

    let expr = (Cst 1) .+. (Cst 2)
    val <- liftIO $ (X86.compile (compileExprInt expr) :: IO Word64) 
    val @?= 3 

    let expr = (Cst 1) .+. ((Cst 5) .-. (Cst 2))
    val <- liftIO $ (X86.compile (compileExprInt expr) :: IO Word64) 
    val @?= 4 
