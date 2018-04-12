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
eval envRef (List [Atom "set!", Atom var, form]) = eval envRef form >>= setVar envRef var 
eval envRef (List [Atom "define", Atom var, form]) = eval envRef form >>= defineVar envRef var
eval envRef (List (Atom "define" : List (Atom fname : params) : body)) =
  defineVar envRef fname $ makeFunc params Nothing body envRef
eval envRef (List (Atom "define" : (DottedList (Atom fname : params) varargs) : body)) =
  defineVar envRef fname $ makeFunc params (Just $ show varargs) body envRef
eval envRef (List (Atom "lambda" : List params : body)) =
  return $ makeFunc params Nothing body envRef
eval envRef (List (Atom "lambda" : (DottedList params varargs) : body)) =
  return $ makeFunc params (Just $ show varargs) body envRef
eval envRef (List (Atom "lambda" : varargs@(Atom _) : body)) =
  return $ makeFunc [] (Just $ show varargs) body envRef
eval envRef (List (function : args)) = do {
    f  <- eval envRef function;
    as <- mapM (eval envRef) args;
    apply f as;
}
eval envRef badForm = throwError $ BadFormError badForm

-- MakeFunc
makeFunc :: [LispVal] -> Maybe String -> [LispVal] -> EnvRef -> LispVal
makeFunc params = Func (map show params)

-- Apply
apply :: LispVal -> [LispVal] -> IOThrowable LispVal
apply (PrimitiveFunc func) args = liftThrowable $ func args
apply (Func params varargs body closure) args = 
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgError (num params) args
    else (bindTo closure paramArgs) >>= evaluate body
  where num = toInteger . length
        bindTo envRef = liftIO . bindVars envRef
        evaluate body envRef = liftM last $ mapM (eval envRef) body
        remainingArgs = drop (length params) args
        varargsArgs = case varargs of
                          Nothing -> []
                          Just v  -> [(v, List $ remainingArgs)]
        paramArgs = zip params args ++ varargsArgs
