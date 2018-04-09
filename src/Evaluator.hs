module Evaluator (eval) where

import LispVal
import Functions

eval :: LispVal -> LispVal
eval (List (Atom funcName : args)) = apply funcName $ map eval args
eval lispVal = lispVal
