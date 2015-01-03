{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns, GeneralizedNewtypeDeriving, LambdaCase #-}
module Typing.Subst where

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



getSubst :: Env Subst
getSubst = lift get

addSubst :: TyVarId -> Type -> Env ()
addSubst n t =
  lift $ modify (MS.insert n t)
  
simpl :: Type -> Env Type
simpl ty =
  do sub <- getSubst
     return $ apply sub ty
  where
    apply s t@(TyVar n) = fromMaybe t $ liftM (apply s) (MS.lookup n s)
    apply s t = t

canon :: Type -> Env Type
canon t = 
   simpl t >>=
     \case
       (t1 :-> t2)   -> liftM2 (:->) (canon t1) (canon t2)
       TyList t      -> liftM TyList (canon t)
       t@(TyConst _) -> return $ t
       t -> return t

occurs :: TyVarId -> Type -> Env Bool
occurs z t =
  (foldr (\x b -> b || (x == z)) False)
  <$> (canon t)

unify :: Type -> Type -> Env ()
unify t1' t2' =
  do t1 <- simpl t1'
     t2 <- simpl t2'
     unify' t1 t2 
  where
    unify' t1@(TyVar a) t2 =
      if  t1 == t2
      then return ()
      else 
        do b <- occurs a t2
           if not b then  addSubst a t2 else error "cyclic type :(" 
    unify' t1 t2@(TyVar _) = unify t2 t1
    unify' (a :-> b) (x :-> y) =
      do unify a x 
         unify b y
    unify' (TyList x) (TyList y) = unify x y
    unify' (TyConst a) (TyConst b) =
      if a == b
      then return ()
      else error $ "can't unify : " ++ show (a,b)
    unify' t1 t2 =
      error $ "can't unify\n" ++ show t1 ++ "\n" ++ show t2 ++ "\n"

