module Lib
  ( Solvable,
    part1,
    part2,

    setDebug,
    getDebug,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

class Solvable a where
  part1 :: a -> String -> String
  part2 :: a -> String -> String

-- Debug Flag

{-# NOINLINE debugRef #-}
debugRef :: IORef Bool
debugRef = unsafePerformIO (newIORef False)

setDebug :: Bool -> IO ()
setDebug = writeIORef debugRef

{-# NOINLINE getDebug #-}
getDebug :: Bool
getDebug = unsafePerformIO (readIORef debugRef)
