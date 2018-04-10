module Variables (getVar, setVar, defineVar) where

import Data.IORef
import Control.Monad.Except

import LispVal
import Environment
import IOThrowable

isBound :: EnvRef -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . lookup var >>=
                     return . maybe False (const True)

getVar :: EnvRef -> String -> IOThrowable LispVal
getVar envRef var = liftIO $ readIORef envRef >>=
                    return . lookup var >>=
                    maybe (return $ Bool False) (liftIO . readIORef)

setVar :: EnvRef -> String -> LispVal -> IOThrowable LispVal
setVar envRef var val = do {
    env <- liftIO $ readIORef envRef;
    liftIO $ maybe (return ()) (flip writeIORef val) (lookup var env);
    return val;
}

defineVar :: EnvRef -> String -> LispVal -> IOThrowable LispVal
defineVar envRef var val = do {
    alreadyDefined <- liftIO $ isBound envRef var;
    if alreadyDefined
        then setVar envRef var val
        else liftIO $ do {
            valRef <- newIORef val;
            env <- readIORef envRef;
            writeIORef envRef ((var, valRef):env);
            return val;
        }
}
