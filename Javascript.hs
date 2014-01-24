module Javascript (javascript) where

import Parse

import Data.List (intercalate)


sepList sep = intercalate sep . map expr

expr :: Expr -> String
expr (LNumber _ s) = s
expr (LString _ s) = "\"" ++ s ++ "\""
expr (LObject _ fields) = "{" ++ intercalate "," (map exprField fields) ++ "}"
  where exprField (s, a) = s ++ ":" ++ expr a
expr (EVar _ s) = s
expr (EApp _ a args) = expr a ++ "(" ++ sepList "," args ++ ")"
expr (EFn _ args a) = "function (" ++ intercalate "," args ++ ") {" ++ exprBlock a ++ "}"
  where exprBlock = intercalate "\n" . map stmt
expr (EField _ e f) = expr e ++ "." ++ f
   
stmt :: Stmt -> String
stmt (SExpr _ a) = expr a
stmt (SBind _ id a) = "var " ++ id ++ " = " ++ expr a ++ ";"
stmt (SReturn _ e) = "return " ++ expr e ++ ";"

javascript :: Block -> String
javascript = intercalate "\n" . map stmt
