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
                [ simpleExpr 
                , addrExpr   ]

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

addrExpr :: TestTree
addrExpr = testCase "Loading and moving things at addr" $ do
    -- 64 bits = 8 bytes = 4 hexadecimal digits
    -- 32 bits = 4 bytes = 2 hexadecimal digits
    let program = do
            lea rax $ addr64 $ rbp - 20
            ret

    retVal <- liftIO $ (compile program :: IO Word64)        
    -- This is meant to force evaluation of retVal
    (const 0 $! retVal) @?= 0

    let program = do
            mov (addr64 $ rbp - 20) 234 -- put value 234 at stack position 20 
            mov rax $ addr64 $ rbp -20
            ret

    retVal <- liftIO $ (compile program :: IO Word64)        
    retVal @?= 234

    -- 64 bits = 8 bytes = 4 hexadecimal digits
    -- 32 bits = 4 bytes = 2 hexadecimal digits

    let program = do
            mov (addr64 $ rbp - 20) 314 -- store some value at position 20 on the stack
            lea rcx $ addr64 $ rbp - 20 -- get address where the value is located
            mov (addr64 $ rbp - 28) rcx -- place address on the stack at position 20 + 8 (64 bit address)

            mov rcx (addr64 $ rbp - 28) -- load address in rcx
            mov rax (addr64 $ rcx)      -- load value at address in a
            ret

    retVal <- liftIO $ (compile program :: IO Word64)        
    retVal @?= 314

