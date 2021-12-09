{-
Module for everything static
-}
module IR.Backend.Static where

import IR.Syntax

import qualified Data.Map as Map
import Control.Monad

checkLinkInScope 
    :: (Ord label) 
    => Module label 
    -> IRInstr label 
    -> [label]
checkLinkInScope mainModule scope = case (stripLoc scope) of
    JComp _ _ _ l  -> if l `Map.member` mainModule then [] else [l]
    _              -> []

-- | Return unknown labels
checkLink ::
     (Ord label, Monad m)
  => Module label 
  -> ([label] -> m ())
  -> m ()
checkLink main callError = do
    let labels = mconcat $ [ checkLinkInScope main instruction 
                           | instructions <- Map.elems main 
                           , instruction  <- instructions   ]

    when (not $ null labels) $
        callError labels

