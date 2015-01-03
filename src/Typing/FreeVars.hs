{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns, GeneralizedNewtypeDeriving #-}
module Typing.FreeVars where

import qualified Data.Map.Strict as MS
import Data.Traversable
import Control.Applicative
import Control.Monad hiding (mapM, mapM_)
import Data.Foldable
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad.Identity hiding (mapM,mapM_)
import Control.Monad.Writer.Strict hiding (mapM,mapM_)
import Prelude hiding (foldr,foldl,concatMap,mapM,mapM_)

import Typing.Definitions
import Typing.Subst

fvars :: Type -> Env (S.Set TyVarId)
fvars t' =
  do t <- canon t'
     return $ foldr S.insert S.empty t

getFVars :: Env (S.Set TyVarId)
getFVars =
  do fv <- lift . lift $ get
     fv'<- foldr'
           (\tyId set ->
             liftM2 S.union set (fvars . TyVar $ tyId)
           )
           (return $ S.empty)
           fv 
     lift . lift $ put fv'
     return fv'

addFvar :: TyVarId -> Env ()
addFvar id =
  lift $ lift $ modify (S.insert id)

addFvars :: (S.Set TyVarId) -> Env ()
addFvars s =
  lift $ lift $ modify (S.union s)

removeFvars :: (S.Set TyVarId) -> Env ()
removeFvars s =
  lift $ lift $ modify (S.\\ s)          
