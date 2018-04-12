module Evaluator (eval) where

import Control.Monad.Except

import CoreDataTypes
import Variables

-- Eval
eval :: EnvRef -> LispVal -> IOThrowable LispVal
eval envRef var@(Number _) = return var
eval envRef var@(Bool _) = return var
eval envRef var@(PrimitiveFunc _) = return var
eval envRef (Atom var) = getVar envRef var
eval envRef (List [Atom "if", ifForm, thenForm, elseForm]) = do {
    ifBool <- eval envRef ifForm;
    case ifBool of
        Bool True  -> eval envRef thenForm
        _          -> eval envRef elseForm
}
eval envRef var@(DottedList _ _) = return var
eval envRef (List [Atom "set!", Atom var, form]) = eval envRef form >>= setVar envRef var 
eval envRef (List [Atom "define", Atom var, form]) = eval envRef form >>= defineVar envRef var
eval envRef (List (Atom "define" : List (Atom fname : params) : body)) =
  defineVar envRef fname $ Func (map show params) body envRef
eval envRef (List (Atom "lambda" : List params : body)) =
  return $ Func (map show params) body envRef
eval envRef (List (function : args)) = do {
    f  <- eval envRef function;
    as <- mapM (eval envRef) args;
    apply f as;
}
eval envRef badForm = throwError $ BadFormError badForm

-- Apply
apply :: LispVal -> [LispVal] -> IOThrowable LispVal
apply (PrimitiveFunc func) args = liftThrowable $ func args
apply (Func params body closure) args = 
  if num params /= num args
    then throwError $ NumArgError (num params) args
    else (bindTo closure paramArgs) >>= evaluate body
  where num = toInteger . length
        bindTo envRef = liftIO . bindVars envRef
        evaluate body envRef = liftM last $ mapM (eval envRef) body
        paramArgs = zip params args
