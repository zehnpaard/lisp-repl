module Variables (getVar, setVar, defineVar, bindVars) where

import Data.IORef
import Control.Monad.Except

import CoreDataTypes

isBound :: EnvRef -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . lookup var >>=
                     return . maybe False (const True)

getVar :: EnvRef -> String -> IOThrowable LispVal
getVar envRef var = do {
    env <- liftIO $ readIORef envRef;
    maybe (throwError $ UnboundVar var) (liftIO . readIORef) (lookup var env);
}

setVar :: EnvRef -> String -> LispVal -> IOThrowable LispVal
setVar envRef var val = do {
    env <- liftIO $ readIORef envRef;
    maybe (throwError $ UnboundVar var) (liftIO . flip writeIORef val) (lookup var env);
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

bindVars :: EnvRef -> [(String, LispVal)] -> IO EnvRef
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBindings bindings)
        addBindings (var, val) = do ref <- newIORef val
                                    return (var, ref)
