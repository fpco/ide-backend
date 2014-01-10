{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, CPP #-}
module A where

import Language.Haskell.TH

foreign import ccall meaningOfLife :: IO Int

ex1 :: Q Exp
ex1 = [| \x -> print =<< x |]

ex2 :: Q Exp
ex2 =
#if !MIN_VERSION_base(999,0,0)
  [| meaningOfLife |]
#else
  "terrible error"
#endif
