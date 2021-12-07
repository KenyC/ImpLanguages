module IR.Backend.Asm.RunCode where

import Data.Word
import Foreign.Ptr

import CodeGen.X86

foreign import ccall "dynamic" callIOVal :: FunPtr (IO Word64) -> IO Word64
instance Callable (IO Word64) where
  dynCCall = callIOVal

foreign import ccall "dynamic" callIOUnit :: FunPtr (IO ()) -> IO ()
instance Callable (IO ()) where
  dynCCall = callIOUnit

foreign import ccall "dynamic" callCFun_ :: FunPtr (Word64 -> Word64) -> Word64 -> Word64


sizeVar :: Word64
sizeVar = 8 -- 8 bytes, 64 bits



foreign import ccall safe "static stdlib.h &malloc"
  c_malloc :: FunPtr (Word64 -> IO Word64)

callMalloc :: Operand RW S64 -> Code
callMalloc r = callFun r c_malloc

foreign import ccall safe "static stdlib.h &free"
  c_free :: FunPtr (Word64 -> IO ())

callFree :: Operand RW S64 -> Code
callFree r = callFun r c_free


foreign import ccall unsafe "static stdlib.h &abs"
  c_abs :: FunPtr (Word64 -> Word64)

callAbs :: Operand RW S64 -> Code
callAbs r = callFun r c_abs

