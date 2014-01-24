module TypeCheck.Types where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (intercalate)
import Data.Maybe (fromMaybe)


data Type = TVar String
          | TConstructor String
          | TFun Type [Type] Type
          | TObject (M.Map String Type) (Maybe String)
          deriving (Eq)

data Scheme = Scheme [String] Type deriving (Show)

instance Show Type where
  show (TVar s) = "?" ++ s
  show (TConstructor s) = s
  show (TFun (TVar _) a b) = "(" ++ intercalate "," (map show a) ++ ") -> " ++ show b
  show (TFun this a b) = show this ++ "(" ++ intercalate "," (map show a) ++ ") -> " ++ show b
  show (TObject fields var) = "{" ++ intercalate "," (map showField $ M.assocs fields) ++ (ext var) ++ "}"
    where showField (s, a) = s ++ ":" ++ show a
          ext (Just _) = "..."
          ext Nothing = ""

-- Simple types
  
tBool = TConstructor "Boolean"
tNum = TConstructor "Number"
tString = TConstructor "String"
tUnit = TConstructor "Unit"


-- Substitutions: A mapping of variable names to types.

type Subst = M.Map String Type

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (substitute s1) s2 `M.union` s1


-- Environment: A map of identifiers to schemes. Schemes are
-- generalised types: they have free type variables that do not appear
-- in the environment. Schemes can be instantiated back into types
-- during the inference process - see below.

newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = S.toList (ftv t `S.difference` ftv env)


-- Types class: A type with the 'Types' class has a set of free type
-- variables and can be applied to a substition.

class Types a where
  ftv :: a -> S.Set String
  substitute :: Subst -> a -> a

instance Types Type where
  ftv (TVar n) = S.singleton n
  ftv (TConstructor _) = S.empty
  ftv (TFun t1 t2 t3) = ftv t1 `S.union` ftv t2 `S.union` ftv t3
  ftv (TObject r (Just v)) = (ftv (M.elems r)) `S.union` (S.singleton v)
  ftv (TObject r Nothing) = ftv (M.elems r)

  substitute s (TVar n) = fromMaybe (TVar n) (M.lookup n s)
  substitute s (TFun t1 t2 t3) = TFun (substitute s t1) (substitute s t2) (substitute s t3)
  
  -- Substitutions for objects need to take into account the row
  -- variable. If the substitution is an object, the result is the
  -- union of the two sets of fields. The row variable may be replaced
  -- with another variable. Any other type is an error.
  substitute s (TObject r (Just v)) = case M.lookup v s of
    Just (TObject r' v') -> TObject (M.union (M.map (substitute s) r) r') v'
    Just (TVar v') -> TObject (M.map (substitute s) r) (Just v')
    Just t -> error $ "Cannot substitute non-row type " ++ show t ++ " into object."
    Nothing -> TObject (M.map (substitute s) r) (Just v)
  substitute s (TObject r Nothing) = TObject (M.map (substitute s) r) Nothing

  substitute s t = t

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `S.difference` S.fromList vars
  substitute s (Scheme vars t) = Scheme vars (substitute (foldr M.delete s vars) t)

instance Types TypeEnv where
  ftv (TypeEnv env) =  ftv (M.elems env)
  substitute s (TypeEnv env) =  TypeEnv (M.map (substitute s) env)

instance Types a => Types [a] where
  substitute s = map (substitute s)
  ftv = foldr (S.union . ftv) S.empty
