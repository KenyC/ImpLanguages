module IR.Loc where

import Data.Map (Map)
import qualified Data.Map as Map 

data IRLoc label = IRLoc {
    scope :: label,
    pos   :: Int
} deriving (Eq, Ord, Show)

class LocStrippable a where
    stripLoc :: a -> a

instance (LocStrippable a) => LocStrippable [a] where 
    stripLoc = map stripLoc

instance (LocStrippable a) => LocStrippable (Map k a) where 
    stripLoc = fmap stripLoc

instance (LocStrippable a) => LocStrippable (Maybe a) where 
    stripLoc = fmap stripLoc