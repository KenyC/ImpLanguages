module Pretty where

class Pretty a where
    prettyShowPrec :: a -> (String, Int)

prettyShow :: (Pretty a) => a -> String
prettyShow = fst . prettyShowPrec


prettyPrint :: (Pretty a) => a -> IO () 
prettyPrint = putStrLn . prettyShow 