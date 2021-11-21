import Test.Tasty

import qualified Test.IR.Backend.Haskell
import qualified Test.IR.Syntax

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.IR.Syntax.allTests 
            , Test.IR.Backend.Haskell.allTests ]


