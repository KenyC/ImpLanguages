import Test.Tasty

import qualified Test.IR.Backend.Haskell

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.IR.Backend.Haskell.allTests ]


