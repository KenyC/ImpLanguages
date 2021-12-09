module Test.IR.Backend.Static where

import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map


import Pretty
import IR.Syntax
import IR.Program
import IR.Backend.Static

allTests :: TestTree
allTests = testGroup 
                "Static compiler check"
                [ inexistentLabels 
                , everythingOk     ]


inexistentLabels, everythingOk :: TestTree

inexistentLabels = testCase "Inexistent Labels" $ do
    let program = mkProg $ do
             jump "Outer Space"

             "Africa" ~> do
                Allocate (Cst 5) *= Cst 4

             "Asia" ~> do
                Allocate (Cst 5) *= Cst 4
                jump "Africa"

    checkLink program (const Nothing) @?= (Nothing :: Maybe ())



everythingOk = testCase "All link exists" $ do
    let program = mkProg $ do
             jump "Africa"

             "Africa" ~> do
                Allocate (Cst 5) *= Cst 4

             "Asia" ~> do
                Allocate (Cst 5) *= Cst 4
                jump "Africa"

    checkLink program (const Nothing) @?= Just ()

    let program = mkProg $ do
            jump "existing_label"

            "existing_label" ~> do
                return ()

    checkLink program (const Nothing) @?= Just ()
    
    return ()