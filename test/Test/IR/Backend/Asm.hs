module Test.IR.Backend.Asm where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import CodeGen.X86 hiding (compile)
import qualified CodeGen.X86 as X86

import IR.Syntax
import IR.Program
import IR.Backend.Asm
import IR.Backend.Asm.RunCode


allTests :: TestTree
allTests = testGroup 
                "Assembly Runtime Backend"
                [ simpleExprTest 
                , addrExprTest   ]

simpleExprTest :: TestTree
simpleExprTest = testCase "Simple constant expr" $ do
    let nMap = Map.empty
    let expr = Cst 1
    -- liftIO $ print $ selfContainedExpr nMap expr
    val <- liftIO $ (X86.compile (selfContainedExpr nMap expr) :: IO Word64) 
    val @?= 1 

    let expr = (Cst 1) .+. (Cst 2)
    val <- liftIO $ (X86.compile (selfContainedExpr nMap expr) :: IO Word64) 
    val @?= 3 

    let expr = (Cst 1) .+. ((Cst 5) .-. (Cst 2))
    val <- liftIO $ (X86.compile (selfContainedExpr nMap expr) :: IO Word64) 
    val @?= 4

    let expr = ((Cst 432) .-. (Cst 1)) .+. ((Cst 5) .-. (Cst 2))
    val <- liftIO $ (X86.compile (selfContainedExpr nMap expr) :: IO Word64) 
    val @?= 434

addrExprTest :: TestTree
addrExprTest = testCase "Addr expr" $ do
    let placeValueOnStackAt :: Word64 -> Word64 -> X86.Code 
        placeValueOnStackAt pos value = 
            mov (addr64 $ rbp - (fromIntegral $ pos * sizeVar)) (fromIntegral value)

    let loadStackAddrAt ::  Word64 -> Word64 -> X86.Code
        loadStackAddrAt posLoad posFrom = do
            push rcx
            lea 
               rcx
               (addr64 $ rbp - (fromIntegral $ posFrom * sizeVar))
            mov
               (addr64 $ rbp - (fromIntegral $ posLoad * sizeVar))  
               rcx
            pop rcx

        -- expr = Cst 23
    
    let var :: IRName = 1
    let expr :: IRExpr 'IntTy 
        expr = Deref (Var var)
    let posVar = 3
    let nameMap = Map.fromList [(var, posVar)]

    let code = saveNonVolatile $ do
            placeValueOnStackAt 1 321
            loadStackAddrAt posVar 1 -- loading the address of 1 into position 3 (i.e. $3 = &$1) 
            compileExpr nameMap expr


    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 321

    -- a complex expression featuring push and move from local stack
    let expr :: IRExpr 'IntTy 
        expr = Cst 1 .+. (Deref (Var var) .+. (Cst 1 .+. Deref (Var var)))
    let code = saveNonVolatile $ do
                    -- prologue
                    mov rbp rsp
                    sub rsp (fromIntegral $ 10 * sizeVar)

                    placeValueOnStackAt 1 20
                    loadStackAddrAt posVar 1 -- loading the address of 1 into position 3 (i.e. $3 = &$1) 
                    compileExpr nameMap expr

                    -- epilogue
                    mov rsp rbp


    -- liftIO $ print code
    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 42
    return ()
