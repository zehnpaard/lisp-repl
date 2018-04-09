module Evaluator (eval) where

import LispVal
import Functions

eval :: LispVal -> LispVal
eval (List [Atom "if", ifForm, thenForm, elseForm]) = 
  case eval ifForm of
    Bool True  -> eval thenForm
    Bool False -> eval elseForm
eval (List (Atom funcName : args)) = apply funcName $ map eval args
eval lispVal = lispVal
