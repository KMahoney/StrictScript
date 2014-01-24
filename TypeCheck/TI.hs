-- | TI Monad. Maintain state for a supply of 'fresh' variables and
-- the current substitution. Type inference can fail and return an
-- error message.
module TypeCheck.TI where

import TypeCheck.Types

import qualified Data.Map as M
import Data.Functor ((<$>))

import Control.Monad.Error
import Control.Monad.State


data TIState = TIState {tiSupply :: Int, tiCurrent :: Subst} deriving (Show)
type TIError = String

type TI a = ErrorT TIError (State TIState) a

runTI :: TI a -> (Either TIError a, TIState)
runTI t = runState (runErrorT t) TIState{tiSupply = 0, tiCurrent = nullSubst}

freshTVar :: String -> TI Type
freshTVar prefix = do
  s <- get
  put $ s {tiSupply = tiSupply s + 1}
  return $ TVar $ prefix ++ show (tiSupply s)

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do 
  nvars <- mapM (const $ freshTVar "_") vars
  let s = M.fromList (zip vars nvars)
  return $ substitute s t
  
-- | Extends the current subsitution. Note that 'composeSubst' is not
-- symmetric -- the algorithm doesn't work if the arguments are
-- reversed.  Rule: new-subst `composeSubst` old-subst
extSubst :: Subst -> TI ()
extSubst sub = do
  st <- get
  put $ st {tiCurrent = sub `composeSubst` tiCurrent st}
  
-- | The current substitution.
getSubst :: TI Subst
getSubst = tiCurrent <$> get
