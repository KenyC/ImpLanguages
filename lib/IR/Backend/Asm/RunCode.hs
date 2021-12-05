module IR.Backend.Asm.RunCode where

import Data.Word
import Foreign.Ptr

import CodeGen.X86

foreign import ccall "dynamic" callIOVal :: FunPtr (IO Word64) -> IO Word64
instance Callable (IO Word64) where
  dynCCall = callIOVal

