module Evaluator (eval) where

import Control.Monad.Except

import LispVal
import LispError
import Environment
import Functions
import Variables
import IOThrowable

eval :: EnvRef -> LispVal -> IOThrowable LispVal
eval envRef var@(Number _) = return var
eval envRef var@(Bool _) = return var
eval envRef (Atom var) = getVar envRef var
eval envRef (List [Atom "if", ifForm, thenForm, elseForm]) = do {
    ifBool <- eval envRef ifForm;
    case ifBool of
        Bool True  -> eval envRef thenForm
        Bool False -> eval envRef elseForm
}
eval envRef (List [Atom "set!", Atom var, form]) = eval envRef form >>= setVar envRef var 
eval envRef (List [Atom "define", Atom var, form]) = eval envRef form >>= defineVar envRef var
eval envRef (List (Atom funcName : args)) = mapM (eval envRef) args >>= liftThrowable . apply funcName
eval envRef badForm = throwError $ BadFormError badForm
