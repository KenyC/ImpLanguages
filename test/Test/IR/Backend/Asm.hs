module Test.IR.Backend.Asm where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import Data.List (nub)
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
                , addrExprTest   
                , nameMapTest  
                , offsetExprTest   ]

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

    
    let var :: IRName = 1
    let expr :: IRExpr 'IntTy 
        expr = Deref (Var var)
    let posVar = 3
    let nameMap = Map.fromList [(var, posVar)]

    let code = makeSelfContained nameMap $ do
                    placeValueOnStackAt 1 321
                    loadStackAddrAt posVar 1 -- loading the address of 1 into position 3 (i.e. $3 = &$1) 
                    compileExpr nameMap expr


    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 321

    -- a complex expression featuring push and move from local stack
    let expr :: IRExpr 'IntTy 
        expr = Cst 1 .+. (Deref (Var var) .+. (Cst 1 .+. Deref (Var var)))
    let code = makeSelfContained nameMap $ do
                    placeValueOnStackAt 1 20
                    loadStackAddrAt posVar 1 -- loading the address of 1 into position 3 (i.e. $3 = &$1) 
                    compileExpr nameMap expr



    -- liftIO $ print code
    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 42
    return ()




offsetExprTest :: TestTree
offsetExprTest = testCase "Offset expr" $ do
    
    let var :: IRName = 1
    let expr :: IRExpr 'IntTy 
        expr = Deref (Var var `Offset` Cst 1)

    let posVar = 5
    let nameMap = Map.fromList [(var, posVar)]

    let code = makeSelfContained nameMap $ do
                    placeValueOnStackAt 1 321
                    placeValueOnStackAt 2 434
                    loadStackAddrAt posVar 2 -- loading the address of 1 into position 3 (i.e. $3 = &$1) 
                    compileExpr nameMap expr


    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 321


    let expr :: IRExpr 'IntTy 
        expr = Deref (Var var `Offset` Cst 1) .+. Deref (Var var `Offset` Cst 2)


    let code = makeSelfContained nameMap $ do
                    placeValueOnStackAt 1 2
                    placeValueOnStackAt 2 3
                    placeValueOnStackAt 3 4
                    loadStackAddrAt posVar 3 -- loading the address of 1 into position 3 (i.e. $3 = &$1) 
                    compileExpr nameMap expr


    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 5


nameMapTest :: TestTree
nameMapTest = testCase "Make name map test" $ do
    let program = mkProg $ do
            0 .= Cst 1 .+. (Deref $ Var 3)
            (Var 4) *= (Allocate (Deref $ Var 3))

            "scope1" ~> do
                4 .= Allocate (Deref $ Var 5)

    let nameMap = makeNameMap program

    Set.fromList (Map.keys nameMap) @?= Set.fromList [0, 3, 4, 5]

    -- no duplicates
    length (Map.keys nameMap) @?= length (nub $ Map.elems nameMap) 

    -- minimal
    maximum nameMap @?= (fromIntegral $ Map.size nameMap) 

------------------- UTILS -----------------

placeValueOnStackAt :: Word64 -> Word64 -> X86.Code 
placeValueOnStackAt pos value = 
    mov (addr64 $ rbp - (fromIntegral $ pos * sizeVar)) (fromIntegral value)

loadStackAddrAt ::  Word64 -> Word64 -> X86.Code
loadStackAddrAt posLoad posFrom = do
    push rcx
    lea 
       rcx
       (addr64 $ rbp - (fromIntegral $ posFrom * sizeVar))
    mov
       (addr64 $ rbp - (fromIntegral $ posLoad * sizeVar))  
       rcx
    pop rcx