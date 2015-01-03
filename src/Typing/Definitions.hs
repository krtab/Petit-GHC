{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns, GeneralizedNewtypeDeriving #-}
module Typing.Definitions where

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

import AST
import Lexer


type TyVarId = Int


data TypeG a = TyVar !a
             | !(TypeG a) :-> !(TypeG a)
             | TyList !(TypeG a)
             | TyConst !TyConst
          deriving (Show,Eq,Functor,Foldable,Traversable)

infixr 3 :->

data TyConst = TyChar | TyInt | TyBool | TIO
                                 deriving (Show,Eq)
                                          

type Type = TypeG TyVarId



newtype PolyTypeG a = PolyType {runPolyType :: Writer (S.Set TyVarId) a}
                 deriving (Monad, Functor, Applicative, MonadWriter (S.Set TyVarId))

type PolyType = PolyTypeG Type

instance Show a => Show (PolyTypeG a) where
  show (runWriter . runPolyType -> (monotype, bindings)) =
    (concatMap (\x -> '∀':(show x)) bindings) ++ ", " ++ show monotype


type Subst = MS.Map TyVarId Type


type Env =
  StateT (MS.Map Ident PolyType) (
    StateT Subst (
       StateT (S.Set TyVarId) (
          State TyVarId
          )
       )
    )

