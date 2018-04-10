module Environment (EnvRef, nullEnvRef) where

import Data.IORef

import LispVal

type EnvRef = IORef [(String, IORef LispVal)]

nullEnvRef :: IO EnvRef
nullEnvRef = newIORef []
