module Test.IR.Syntax where

import Data.Default
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import IR.Syntax
import IR.Backend.Haskell


allTests :: TestTree
allTests = testGroup 
                "Syntax"
                [ multiLabelModule ]


multiLabelModule :: TestTree
multiLabelModule = testCase "Defining multilabel module monadically" $ do
    let program :: Module String
        program = mkProg $ do
             allocate 0
             jump "a"

             "a" ~> do
                allocate 1
                free 2

             "b" ~> do
                0 |= Cst 34

             free 0

    let expected :: Module String
        expected = Map.fromList $
                        [ (def, (Allocate 0 <> (Jump "a")) <> (Free 0))
                        , ("a", Allocate 1 <> (Free 2))
                        , ("b", Set 0 (Cst 34))]
    program @?= expected