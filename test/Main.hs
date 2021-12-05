import Test.Tasty

import qualified Test.IR.Backend.Haskell
import qualified Test.IR.Backend.Asm
import qualified Test.IR.Backend.Asm.RunCode
import qualified Test.IR.Syntax
import qualified Test.Pretty

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.IR.Syntax.allTests  
            , Test.Pretty.allTests
            , Test.IR.Backend.Asm.allTests
            , Test.IR.Backend.Asm.RunCode.allTests
            , Test.IR.Backend.Haskell.allTests  ]


