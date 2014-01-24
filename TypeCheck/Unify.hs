-- | Unification.
--
-- The main difference from standard Algorithm W is that functions are
-- N-arity. As in JavaScript, they are not partially applied by
-- default. Also handles row unification (most general inserter etc.)
module TypeCheck.Unify (unify) where

import TypeCheck.Types
import TypeCheck.TI

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Error

import Text.Parsec (SourcePos)

-- | A nice interface for unifying two types in the TI monad. Makes
-- sure the types are substituted before finding the most general
-- unifier, and then updates the current substitution. A position is
-- needed for a friendly error message in case the types cannot be
-- unified.
unify :: SourcePos -> Type -> Type -> TI ()
unify pos t1 t2 = do
  s <- getSubst
  s' <- catchError (mgu (substitute s t1) (substitute s t2)) $ \e ->
    throwError $ (show pos) ++ ": Type conflict.\n" ++ e
  extSubst s'


-- | Most general unifier. This needs to be in the TI monad as row
-- unification requires the generation of a fresh row variable.
mgu :: Type -> Type -> TI Subst

-- Unifier for N-arity functions.
mgu f1@(TFun this1 a1 r1) f2@(TFun this2 a2 r2) = do
  s1 <- mguargs nullSubst a1 a2
  s2 <- mgu (substitute s1 r1) (substitute s1 r2)
  s3 <- mgu (substitute s2 this1) (substitute s2 this2)
  return $ s3 `composeSubst` s2 `composeSubst` s1
  where mguargs s [] [] = return s
        mguargs _ [] _ = mismatch
        mguargs _ _ [] = mismatch
        mguargs s (hd1:tl1) (hd2:tl2) = do
          s' <- mgu (substitute s hd1) (substitute s hd2)
          mguargs (s' `composeSubst` s) tl1 tl2
        mismatch = throwError $ "Functions do not unify: " ++ show f1 ++ " vs. " ++ show f2
        
-- Row unifier for objects.
mgu (TObject ma e1) (TObject mb e2) = do
  s <- unifyFields nullSubst (M.toList intersectionMap) mb
  ins1 <- inserter e1 e2 aMissing
  ins2 <- inserter e2 e1 bMissing
  return $ ins2 `composeSubst` ins1 `composeSubst` s
  where aMissing = mb `M.difference` ma
        bMissing = ma `M.difference` mb
        intersectionMap = ma `M.intersection` mb
        
        -- If the row is not extendable, check there are no missing
        -- fields.
        inserter Nothing _ missing = if M.null missing
                                       then return M.empty
                                       else throwError $ "Field(s) do not exist in object: " ++ show (M.keys missing)
                                            
        -- If the row is extendable, return the inserter
        -- substitution. The substituted row is itself extendable if
        -- both unfied objects are extendable.
        inserter (Just v) otherVar missing = do
          (TVar f) <- freshTVar "row"
          return $ M.singleton v $ TObject missing (otherVar >> Just f)
        
        unifyFields s [] _ = return s
        unifyFields s ((key, val):tl) otherMap = case M.lookup key otherMap of
          Just val' -> do
            s' <- mgu (substitute s val) (substitute s val')
            unifyFields (s' `composeSubst` s) tl otherMap
          Nothing -> throwError $ "Missing field: " ++ key

-- Bind types to type variables
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t

-- Simple constructor check
mgu (TConstructor c1) (TConstructor c2) | c1 == c2 = return nullSubst

-- Mismatch
mgu t1 t2 = throwError $ "Types do not unify: " ++ show t1 ++ " vs. " ++ show t2

-- Bind types to type variables. Includes occur check.
varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u = return nullSubst
            | otherwise = return $ M.singleton u t
