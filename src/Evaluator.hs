module Evaluator (eval) where

import LispVal
import Environment
import Functions
import Variables

eval :: EnvRef -> LispVal -> IO LispVal
eval envRef (Atom var) = getVar envRef var
eval envRef (List [Atom "if", ifForm, thenForm, elseForm]) = do {
    ifBool <- eval envRef ifForm;
    case ifBool of
        Bool True  -> eval envRef thenForm
        Bool False -> eval envRef elseForm
}
eval envRef (List [Atom "set!", Atom var, form]) = eval envRef form >>= setVar envRef var 
eval envRef (List [Atom "define", Atom var, form]) = eval envRef form >>= defineVar envRef var
eval envRef (List (Atom funcName : args)) = mapM (eval envRef) args >>= return . apply funcName
eval envRef lispVal = return lispVal
