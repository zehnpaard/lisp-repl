module Evaluator (eval) where

import Control.Monad.Except

import CoreDataTypes
import Apply
import Variables

eval :: EnvRef -> LispVal -> IOThrowable LispVal
eval envRef var@(Number _) = return var
eval envRef var@(Bool _) = return var
eval envRef var@(PrimitiveFunc _) = return var
eval envRef (Atom var) = getVar envRef var
eval envRef (List [Atom "if", ifForm, thenForm, elseForm]) = do {
    ifBool <- eval envRef ifForm;
    case ifBool of
        Bool True  -> eval envRef thenForm
        Bool False -> eval envRef elseForm
}
eval envRef (List [Atom "set!", Atom var, form]) = eval envRef form >>= setVar envRef var 
eval envRef (List [Atom "define", Atom var, form]) = eval envRef form >>= defineVar envRef var
eval envRef (List (function : args)) = do {
    f  <- eval envRef function;
    as <- mapM (eval envRef) args;
    apply f as;
}
eval envRef badForm = throwError $ BadFormError badForm
