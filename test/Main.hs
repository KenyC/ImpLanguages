import Test.Tasty

import qualified Test.IR.Backend.Haskell
import qualified Test.IR.Syntax
import qualified Test.Pretty

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.IR.Syntax.allTests  
            , Test.Pretty.allTests
            , Test.IR.Backend.Haskell.allTests  ]


