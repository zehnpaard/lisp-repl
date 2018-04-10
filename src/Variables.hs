module Variables (getVar, setVar, defineVar) where

import Data.IORef

import LispVal
import Environment

isBound :: EnvRef -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . lookup var >>=
                     return . maybe False (const True)

getVar :: EnvRef -> String -> IO LispVal
getVar envRef var = readIORef envRef >>=
                    return . lookup var >>=
                    maybe (return $ Bool False) readIORef

setVar :: EnvRef -> String -> LispVal -> IO LispVal
setVar envRef var val = do {
    env <- readIORef envRef;
    maybe (return ()) (flip writeIORef val) (lookup var env);
    return val;
}

defineVar :: EnvRef -> String -> LispVal -> IO LispVal
defineVar envRef var val = do {
    alreadyDefined <- isBound envRef var;
    if alreadyDefined
        then setVar envRef var val
        else do {
            valRef <- newIORef val;
            env <- readIORef envRef;
            writeIORef envRef ((var, valRef):env);
            return val;
        }
}
