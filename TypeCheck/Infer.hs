-- | Algorithm W type inference with some changes/extensions:
--
-- * The function type can have more than one argument and a 'this'
-- type.
--
-- * A block statement can have multiple return statements which may
-- terminate the function.
--
-- * Row polymorphism
module TypeCheck.Infer (infer, inferExpr, inferBlock) where

import Parse (Expr(..), Stmt(..), Block(..), parseBlockString, parseExprString)

import TypeCheck.Types
import TypeCheck.TI
import TypeCheck.Unify
import TypeCheck.Core

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Error

import Text.Parsec (SourcePos)

-- | Infer function application.  This is common to both the statement
-- and expression level, so this part of the algorithm is abstracted
-- out.
inferApp :: TypeEnv -> SourcePos -> Expr -> [Expr] -> TI Type

-- Application to a field changes 'this' context. This currently can
-- cause recursive type check failures. Help!
inferApp env _ expr@(EField pos object field) args = do
  objectType <- inferExpr env object
  
  -- Field type must be function
  fnType <- freshTVar "fn"
  (TVar row) <- freshTVar "row"
  unify pos objectType (TObject (M.singleton field fnType) (Just row))
  
  argTypes <- mapM (inferExpr env) args
  ret <- freshTVar "r"
  unify pos (TFun objectType argTypes ret) fnType

  s <- getSubst
  return (substitute s ret)

inferApp env pos expr args = do
  fnType <- inferExpr env expr
  argTypes <- mapM (inferExpr env) args
  this <- freshTVar "this"
  ret <- freshTVar "r"
  unify pos (TFun this argTypes ret) fnType
  s <- getSubst
  return (substitute s ret)


-- | Infer expressions.
inferExpr :: TypeEnv -> Expr -> TI Type

-- Simple literals
inferExpr _ (LNumber _ _) = return tNum
inferExpr _ (LString _ _) = return tString
inferExpr env (LObject pos fields) = do
  fields' <- mapM (\(k,v) -> fmap ((,) k) $ inferExpr env v) fields
  return $ TObject (M.fromList fields') Nothing

-- Variable lookup
inferExpr (TypeEnv env) (EVar pos n) = case M.lookup n env of
  Nothing -> throwError $ show pos ++ " unbound variable: " ++ n
  Just sigma -> instantiate sigma
    
-- Infer function applicaton
inferExpr env (EApp pos expr args) = inferApp env pos expr args
  
-- Infer a function expression
inferExpr (TypeEnv env) (EFn pos arglist body) = do
  argTypes <- mapM (const $ freshTVar "a") arglist
  thisType <- freshTVar "this"
  let argEnv = M.fromList $ zip arglist $ map (Scheme []) argTypes
      thisEnv = M.singleton "this" $ Scheme [] thisType
      env' = TypeEnv $ thisEnv `M.union` argEnv `M.union` env
  ret' <- inferBlock env' [] body
  s <- getSubst
  return $ TFun (substitute s thisType) (substitute s argTypes) ret'

-- Infer field access
inferExpr env (EField pos expr f) = do
  t <- inferExpr env expr
  ret <- freshTVar "r"
  (TVar row) <- freshTVar "row"
  unify pos t (TObject (M.singleton f ret) (Just row))
  s <- getSubst
  return (substitute s ret)


-- | Infer statements.  This collects the types of any return
-- statements in the block and unifies them at the end. If there are
-- no return statements the type of the block defaults to Unit.
inferBlock :: TypeEnv -> [(SourcePos, Type)] -> [Stmt] -> TI Type

-- Unify the accumulated return statements. I'm unsure if this is a
-- sound way of doing this.
inferBlock _ [] [] = return tUnit
inferBlock _ rets [] = do
  ret <- freshTVar "ret"
  mapM_ (\(pos, t) -> unify pos ret t) rets
  s <- getSubst
  return $ substitute s ret

-- Collect return types.
inferBlock env rets (SReturn pos expr:block) = do
  result <- inferExpr env expr
  inferBlock env ((pos, result):rets) block

-- Statement level expression. The return type of the
-- statement is ignored.
inferBlock env rets (SExpr pos expr:block) = do
  inferExpr env expr
  inferBlock env rets block

-- Variable binding.
inferBlock env@(TypeEnv envmap) rets (SBind pos var expr:block) = do
  t <- inferExpr env expr
  s <- getSubst
  let t' = generalize (substitute s env) t
      env' = TypeEnv (M.insert var t' envmap)
  inferBlock env' rets block


-- Public interface
  
globalInfer block = do
  inferBlock (TypeEnv builtins) [] block
        
infer :: Block -> Either TIError Type
infer block = let (result, st) = runTI (globalInfer block) in
  case result of
    Left err -> throwError err
    Right t -> return $ substitute (tiCurrent st) t


