{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns, GeneralizedNewtypeDeriving, LambdaCase #-}

module Typing.Env where

import qualified Data.Map.Strict as MS
import Data.Traversable
import Control.Applicative
import Control.Monad hiding (mapM, mapM_)
import Data.Foldable hiding (find)
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad.Identity hiding (mapM,mapM_)
import Control.Monad.Writer.Strict hiding (mapM,mapM_)
import Prelude hiding (foldr,foldl,concatMap,mapM,mapM_)

import Typing.Definitions
import Typing.FreeVars
import Typing.Subst
import AST
import Lexer

addNoGen :: Ident -> Type -> Env ()
addNoGen id typ =
  do modify (MS.insert id (return $ typ))
     fv <- fvars typ
     addFvars fv

addGen :: Ident -> Type -> Env ()
addGen id typ =
  do enFVars <- getFVars
     fv <- fvars typ
     let gen = fv S.\\ enFVars
     let newPoly = tell gen >>
                   return typ
     modify $ MS.insert id newPoly
     


createVar :: Env TyVarId
createVar =
  lift $ lift $ lift $
  do c <- get
     put (c+1)
     return c
  

find :: Ident -> Env Type
find id =
  do poly <- gets $ (fromMaybe (error $ "pas clé : " ++ id)) . (MS.lookup id)
     polyToMono poly
  where
    polyToMono (runWriter . runPolyType -> (monotype,bindings)) =
      foldr
      (\polyVar t -> liftM2 (\x -> replace polyVar x) (createVar) t)
      (canon monotype)
      bindings
    replace prev new =
      ((\e -> if e == prev then new else e) <$> )


save :: Env (MS.Map Ident PolyType, S.Set TyVarId)
save =
  do m <- get
     s <- lift $ lift $ get
     return (m,s)

restore :: (MS.Map Ident PolyType, S.Set TyVarId) -> Env ()
restore (m,s) =
  do lift $ lift $ put s
     put m

wExp :: Expr AlexPosn -> Env Type
wExp (EApp l e1 e2) =
  do t1 <- wExp e1
     t2 <- wExp e2
     a <- TyVar <$> createVar
     unify t1  (t2 :-> a)
     return a
wExp (EAbstr l id e) =
  do tmp <- save
     a <- TyVar <$> createVar
     addNoGen id a
     t <- wExp e
     restore tmp
     return (a :-> t)
wExp (EIdent l id) =
  find id
wExp (EConst l v) =
  return $ 
  case v of
    (VString _) -> TyList (TyConst TyChar)
    (VInteger _) -> TyConst TyInt
    (VChar _) -> TyConst TyChar
    (VBool _) -> TyConst TyBool
wExp (EITE l eif ethen eelse) =
  do tif <- wExp eif
     unify tif (TyConst TyBool)
     t1 <- wExp ethen
     t2 <- wExp eelse
     unify t1 t2 
     return $ t1
wExp (EUnaryMinus l) =
  return $ 
  (TyConst TyInt) :-> (TyConst TyInt)
wExp (Return l) =
  return $ 
  TyConst TIO
wExp (EOp l op) = 
  let intFunc =
        return $ 
        (TyConst TyInt) :-> (TyConst TyInt) :-> (TyConst TyInt)
      boolFunc =
        return $ 
        (TyConst TyBool) :-> (TyConst TyBool) :-> (TyConst TyBool)
      compFunc =
        return $
        (TyConst TyInt) :-> (TyConst TyInt) :-> (TyConst TyBool)
  in
  case op of
    Plus -> intFunc
    Minus -> intFunc
    Times -> intFunc
    Leq -> compFunc
    Geq -> compFunc
    G -> compFunc
    L -> compFunc
    Neq -> compFunc
    Eq -> compFunc
    And -> boolFunc
    Or -> boolFunc
    Cons ->
      find ":"
    MonadSeq ->
      find ">>"
wExp (ECase l) =
  find "case"
wExp (ELet l defs exp) =
  do tmp <- save
     wScope defs
     t <- wExp exp
     restore tmp
     return t
wExp (EEList l) =
  find "[]"
  
wDef :: Def AlexPosn -> Env (String,Type)
wDef (id,e) =
  do t1 <- wExp e
     t2 <- find id
     unify t1 t2
     t2' <- find id
     return (id,t2)

wScope :: [Def AlexPosn] -> Env ()
wScope file =
   do tmp <- save
      varGiven <- mapM
                  (\def@(id, _) ->
                    do v <- createVar 
                       addNoGen id (TyVar v)
                       return (def,v)
                  )
                  file
     
--      mapM wDef file
   --   results <- mapM (\(id, _) -> (\x -> (id,x)) <$> find id ) file
  --    restore tmp
  --    mapM_ (uncurry addGen) results
     
      mapM_
        (\(def@(id,_),v) -> do
            tmp2 <- save
            (_,t) <- wDef def
            t' <- find id
            unify t' t
            t'' <- find id
            restore tmp2
            removeFvars $ S.singleton v
            addGen id t''
        )
        varGiven
     
      
primitives =
  do addGen "div" $ (TyConst TyInt) :-> (TyConst TyInt) :-> (TyConst TyInt)
     addGen "rem" $ (TyConst TyInt) :-> (TyConst TyInt) :-> (TyConst TyInt)
     addGen "putChar" $ (TyConst TyChar) :-> (TyConst TIO)
     a <- createVar
     removeFvars (S.singleton a) -- ugly hack !!! 
     addGen "error" $ (TyList (TyConst TyChar)) :-> (TyVar a)
     a <- createVar
     removeFvars (S.singleton a) -- ugly hack !!!
     addGen ":" $ (TyVar a) :-> (TyList (TyVar a)) :-> (TyList (TyVar a))
     a <- createVar
     removeFvars (S.singleton a) -- ugly hack !!!
     addGen "[]" $ (TyList (TyVar a))
     a <- createVar
     b <- createVar
     removeFvars (S.fromList [a,b]) -- ugly hack !!!
     addGen "case" $ (TyList (TyVar a)) :-> (TyVar b) :-> ((TyVar a) :-> (TyList (TyVar a)) :-> (TyVar b) ) :-> (TyVar b)
     addGen ">>" $ (TyConst TIO) :-> (TyConst TIO) :-> (TyConst TIO)
     
runEnv env =
  (flip evalState) 0
  $ (flip evalStateT) S.empty
  $ (flip evalStateT) MS.empty
  $ (flip execStateT) MS.empty
  env
