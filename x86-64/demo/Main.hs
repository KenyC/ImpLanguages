{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ViewPatterns      #-}

import           Foreign
import           Data.Vector (Vector, (//))
import           Data.ByteString as BS
import qualified Data.Vector as Vector

import           CodeGen.X86
import           CodeGen.X86.CodeGen

foreign import ccall "dynamic" callWW :: FunPtr (Word64 -> Word64) -> Word64 -> Word64
instance Callable (Word64 -> Word64) where
  dynCCall = callWW

foreign import ccall "dynamic" callPW :: FunPtr (Ptr a -> Word64) -> Ptr a -> Word64
instance Callable (Ptr a -> Word64) where
  dynCCall = callPW

foreign import ccall "dynamic" callIO :: FunPtr (IO ()) -> IO ()
instance Callable (IO ()) where
  dynCCall = callIO

foreign import ccall "wrapper" createPtrWord64_Word64 :: (Word64 -> Word64) -> IO (FunPtr (Word64 -> Word64))
instance CallableHs (Word64 -> Word64) where
  createHsPtr = createPtrWord64_Word64

------------------------------------------------------------------------------ 
-- * examples

-- | Example: identity function in Assembly (look at the source code)
--
-- Input: @rdi@ on Linux \/ System V, @rcx@ on Win64
--
-- Output: @rax@
idCode = saveNonVolatile $ mdo
  mov result 0
  mov ecx 1

  loop <- label
  do
    cmp arg1 0
    j E endLoop

    push result
    add result rcx
    pop rcx

    dec arg1
    jmp loop
  endLoop <- label

  dec arg1


compileToByteArr :: Code -> Vector Word8
compileToByteArr code = let
  (bytes, size) = buildTheCode idCode
  in (Vector.replicate size (1 :: Word8)) // [byte | Right byte <- bytes]

writeByteArr :: FilePath -> Vector Word8 -> IO ()
writeByteArr dest array = do
  BS.writeFile dest $ BS.pack $ Vector.toList array

idFun :: Word64 -> Word64
idFun = compile idCode

main :: IO ()
main = do
    -- callCFun "feezf"
    writeByteArr "test.txt" $ compileToByteArr idCode
    print $ idFun 4
    return ()
