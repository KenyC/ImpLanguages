module Test.IR.Backend.Asm.RunCode where

import Control.Monad.IO.Class
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import CodeGen.X86

import IR.Backend.Asm.RunCode

allTests :: TestTree
allTests = testGroup 
                "Run assembly code (test x86-64)"
                [ simpleExpr ]

simpleExpr :: TestTree
simpleExpr = testCase "Simple constant expr" $ do
    let program = do
            mov rax 345
            ret

    retVal <- liftIO $ (compile program :: IO Word64)        
    retVal @?= 345

    let program = do
            mov rax 345
            add rax 23
            ret

    retVal <- liftIO $ (compile program :: IO Word64)        
    retVal @?= 345 + 23

    let program = do
            mov rcx 332
            mov rax 0

            loop <- label
            do
                dec rcx
                inc rax
                cmp rcx 0
                j NZ loop

            ret

    retVal <- liftIO $ (compile program :: IO Word64)        
    retVal @?= 332
