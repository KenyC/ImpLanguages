module Test.IR.Backend.Asm where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import Data.Default
import Data.List (nub)
import Test.Tasty
import Test.Tasty.HUnit

import CodeGen.X86 hiding (compile)
import qualified CodeGen.X86 as X86

import Pretty
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
                , offsetExprTest   
                , simpleInstrTest   
                , allocateFreeTest  
                , returnInstrTest   
                , simpleJumpTest  
                , complexProgram   ]

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
    maximum nameMap @?= (fromIntegral $ Map.size nameMap) + 1 

simpleInstrTest :: TestTree
simpleInstrTest = testCase "Test simple instructions (w/o allocation)" $ do
    let var1, var2, var3 :: IRName
        var1:var2:var3:_ = [0..]
    let program :: [IRInstr ()]
        program = mkScope $ do
                     var1 .= Cst 3
        nameMap = Map.fromList [(var1, 1)]
        code = makeSelfContained nameMap $ do
                    -- reserve two more spaces on the stack
                    sub rsp $ fromIntegral $ (2 * 8 :: Word64) 
                    -- fill "var1" with address to new spaces
                    loadStackAddrAt 1 3

                    forM_ program $ \instr ->
                        compileInstr def nameMap instr

                    -- return value at stack pos 3
                    mov rax (addrStackPos 3)



    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 3



    let program :: [IRInstr ()]
        program = mkScope $ do
                     var1 .= Cst 3
                     var2 .= (Deref $ Var var1) .+. (Cst 1)
        nameMap = Map.fromList [(var1, 1), (var2, 2)]
        code = makeSelfContained nameMap $ do
                    -- reserve two more spaces on the stack
                    sub rsp $ fromIntegral $ (2 * 8 :: Word64) 
                    -- fill "var1" with address to new spaces
                    loadStackAddrAt 1 3
                    loadStackAddrAt 2 4

                    forM_ program $ \instr ->
                        compileInstr def nameMap instr

                    -- return value at stack pos 3
                    mov rax (addrStackPos 4)



    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 4

    

    let program :: [IRInstr ()]
        program = mkScope $ do
                     var1 .= Cst 34
                     var2 .= Cst 47
                     var1 `is` Var var2
                     var1 .= Cst 75   -- no change in value expected at original pointed adress of "var1"

        nameMap = Map.fromList [(var1, 1), (var2, 2)]
        -- nameMap = Map.fromList [(var1, 1)]
        code = makeSelfContained nameMap $ do
                    -- reserve two more spaces on the stack
                    sub rsp $ fromIntegral $ (2 * 8 :: Word64) 
                    -- fill "var1" with address to new spaces
                    loadStackAddrAt 1 3
                    loadStackAddrAt 2 4

                    forM_ program $ \instr ->
                        compileInstr def nameMap instr

                    -- return value at stack pos 3
                    mov rax (addrStackPos 3)


    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 34

    
allocateFreeTest :: TestTree
allocateFreeTest = testCase "Test allocation and free'ing" $ do

    let var1, var2, var3 :: IRName
        var1:var2:var3:_ = [0..]
    let program :: [IRInstr ()]
        program = mkScope $ do
                     var1 `is` Allocate (Cst 1)
                     var1 .= Cst 345
                     var2 .= Deref (Var var1) `asType` intTy
                     free (Var var1)
        nameMap = Map.fromList [(var1, 1), (var2, 2)]
        code = makeSelfContained nameMap $ do
                    sub rsp $ fromIntegral $ (1 * 8 :: Word64) 
                    loadStackAddrAt 2 3


                    forM_ program $ \instr ->
                        compileInstr def nameMap instr

                    -- return value at stack pos 1
                    mov rax (addrStackPos 3)
                    -- push rax

    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 345

    let program :: [IRInstr ()]
        program = mkScope $ do
                     var1 `is` Allocate (Cst 2)
                     var1 .= Cst 345
                     (Var var1) `Offset` (Cst 1) *= Cst 327
                     var2 .= Deref (Var var1 `Offset` (Cst 1)) `asType` intTy
                     free (Var var1)
        nameMap = Map.fromList [(var1, 1), (var2, 2)]
        code = makeSelfContained nameMap $ do
                    sub rsp $ fromIntegral $ (1 * 8 :: Word64) 
                    loadStackAddrAt 2 3


                    forM_ program $ \instr ->
                        compileInstr def nameMap instr

                    -- return value at stack pos 1
                    mov rax (addrStackPos 3)
                    -- push rax

    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 327


simpleJumpTest :: TestTree
simpleJumpTest = testCase "Test simple jumps" $ do
    let program = mkProg $ do
            jump "inexistent_label"

            "existing_label" ~> do
                return ()

    void (compile program) @?= Left (UndefinedLabels ["inexistent_label"])

    let program = mkProg $ do
            jump "existing_label"

            "existing_label" ~> do
                return ()

    void (compile program) @?= Right ()


    let Right code = compile program
    val <- liftIO $ (X86.compile code :: IO ())
    val `seq` 0 @?= 0  -- just to check that there are no exception


    let program = mkProg $ do
            out $ Cst 4
            jump "existing_label"

            "existing_label" ~> do
                out $ Cst 43

    void (compile program) @?= Right ()


    let Right code = compile program
    val <- liftIO $ (X86.compile code :: IO Word64)
    val @?= 43


returnInstrTest :: TestTree
returnInstrTest = testCase "Test return instructions" $ do

    let program :: Module ()
        program = mkProg $ do
            out $ Cst 1234


    void (compile program) @?= Right ()

    let Right code = compile program
    val <- liftIO $ (X86.compile code :: IO Word64)
    val  @?= 1234   -- just to check that there are no exception


    let program :: Module ()
        program = mkProg $ do
            var <- allocate1_
            var .= Cst 4321
            out $ (*.) var
            free (Var var)


    void (compile program) @?= Right ()

    let Right code = compile program
    val <- liftIO $ (X86.compile code :: IO Word64)
    val  @?= 4321   -- just to check that there are no exception


    let program :: Module ()
        program = mkProg $ do
            var <- allocate1_
            var .= Cst 4321

            var2 <- allocate1_
            var2 .= Cst 1234
            out $ ((*.) var) .+. ((*.) var2)
            free (Var var)


    void (compile program) @?= Right ()

    let Right code = compile program
    val <- liftIO $ (X86.compile code :: IO Word64)
    val  @?= 5555   -- just to check that there are no exception


    let program :: Module ()
        program = mkProg $ do
            var <- allocate1_
            var .= Cst 456
            out $ (*.) var

            var .= Cst 555
            out $ (*.) var
            free (Var var)


    void (compile program) @?= Right ()

    let Right code = compile program
    val <- liftIO $ (X86.compile code :: IO Word64)
    val  @?= 555   -- just to check that there are no exception

complexProgram :: TestTree
complexProgram = testCase "Complex program" $ do
    let fibonacci :: Module String
        fibonacci = mkProg $ do
            n <- allocate1_
            n .= Cst 12

            prev      <- allocate1_
            current   <- allocate1_
            swapBuffer <- allocate1_ 

            prev    .= Cst 0
            current .= Cst 1

            jump "loop"

            "loop" ~> do
                jeq ((*.) n) (Cst 0) "end"

                swapBuffer .= (*.) current `asType` intTy
                current    .= (*.) current .+. (*.) prev 
                prev       .= (*.) swapBuffer `asType` intTy

                n .= (*.) n .-. (Cst 1)
                jump "loop"


            "end" ~> do
                out $ (*.) current
                free $ Var prev
                free $ Var current
                free $ Var swapBuffer
                free $ Var n


    void (compile fibonacci) @?= Right ()

    let Right code = compile fibonacci
    liftIO $ putStrLn ""
    liftIO $ print code
    value <- liftIO $ (X86.compile code :: IO Word64)
    value @?= 234 -- fib 12 = 233



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