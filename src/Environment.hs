module Environment (Env, nullEnv) where

import Data.IORef

import LispVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []
