module Apply ( apply ) where

import CoreDataTypes

apply :: LispVal -> [LispVal] -> IOThrowable LispVal
apply (PrimitiveFunc func) args = liftThrowable $ func args


