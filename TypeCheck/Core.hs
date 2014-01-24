-- | Standard function and operator types.
module TypeCheck.Core (builtins) where

import TypeCheck.Types

import qualified Data.Map as M


-- Built in types

stdlib = [ ("id", Scheme ["a", "b"] (TFun (TVar "b") [TVar "a"] (TVar "a")))
         , ("..", Scheme ["a"] (TFun (TVar "a") [tString, tString] tString))
         , ("console", Scheme ["a", "b"] (TObject (M.fromList [("log", (TFun (TVar "b") [TVar "a"] tUnit))]) Nothing))
         ]

builtins = M.fromList $ stdlib ++ numOps ++ ordOps ++ cmpOps
  where numOp op = (op, Scheme ["a"] (TFun (TVar "a") [tNum, tNum] tNum))
        numOps = map numOp ["+", "-", "*", "/"]
        
        ordOp op = (op, Scheme ["a"] (TFun (TVar "a") [tNum, tNum] tBool))
        ordOps = map ordOp ["<", ">", "<=", ">="]
        
        cmpOp op = (op, Scheme ["a", "b"] (TFun (TVar "b") [TVar "a", TVar "a"] tBool))
        cmpOps = map cmpOp ["==", "!="]
        

