module Evaluator (eval) where

import LispVal
import Environment
import Functions
import Variables

eval :: Env -> LispVal -> IO LispVal
eval env (Atom var) = getVar env var
eval env (List [Atom "if", ifForm, thenForm, elseForm]) = do {
    ifBool <- eval env ifForm;
    case ifBool of
        Bool True  -> eval env thenForm
        Bool False -> eval env elseForm
}
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var 
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom funcName : args)) = mapM (eval env) args >>= return . apply funcName
eval env lispVal = return lispVal
