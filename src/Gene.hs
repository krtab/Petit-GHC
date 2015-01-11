{-# LANGUAGE LambdaCase #-}
module Gene where 
import AST
import MIPS
import Control.Monad.Writer
import Control.Applicative ((<$>))
import Data.List (find,intersperse)
import Control.Monad.State
import qualified Data.Set as S
import Control.Arrow ((***))
import Core
import Data.Char (toLower)

compileSExp' sexp@(SEApp f x) r = do
  (saving [_s1,_s2] $ do
      compileSExp f _s1
      compileSExp x _s2
      requestHeap 4
      li _t0 1
      sw _t0 (At _v0)
      sw _s1 (2 :> _v0)
      sw _s2 (3 :> _v0) )
  move r _v0
compileSExp' (SEConst v) r =
  case v of
    (VString s) ->
      flip compileSExp r $
      foldr (\x l ->
              SEApp
              (SEApp (SEIdent "CoreCons") (SEConst (VChar x)))
               l
              )
      (SEIdent "CoreEmptyList")
      s
    (VInteger n) -> storeCalculated n
    (VChar c) -> storeCalculated (fromEnum c)
    (VBool b) -> storeCalculated (fromEnum b)
  where
    storeCalculated x = do
      l <- newLabel
      putOnData (Word l ["0",show x,"0","0"])
      la r l
compileSExp' SEArg0 r = do
  move r _a0
compileSExp' (SEClotElem n) r = do
  lw r (n :> _a1)
compileSExp' (SEClos l ident) r = do
  (saving [_s1,_s2] $ do
      let n = length l
      requestHeap n
      move _s1 _v0
      let indexed = zip [0..] l
      mapM_ (\(i,e) -> compileSExp e _t1 >> sw _t1 (i :> _s1)) indexed
      requestHeap 2
      move _t6 _v0
      la _t1 (Label ident)
      sw _t1 (At _t6)
      sw _s1 (1 :> _t6))
  storeCalculated _t6
  move r _v0
compileSExp' (SELocal 0 n) r = do
  lw r ((-n) :> _fp)
compileSExp' (SELocal i n) r = saving [_fp] $ do
  lw _fp (At _fp)
  compileSExp (SELocal (i -1) n) r
compileSExp' (SELet l e) r = saving [_fp] $ do
  move _fp _sp
  mapM
    (\e -> do compileSExp e _t1
              decreaseSP 1
              sw _t1 (At _sp)
    )
    l
  compileSExp e r
  mapM_ (const (increaseSP 1)) l
compileSExp' (SEIdent s) r =
  la r (Label s)

compileSExp sexp r = do
  comment $ "begin : " ++ (show sexp) ++ " | " ++ (show r)
  compileSExp' sexp r
  comment $ "end : " ++ (show sexp) ++ " | " ++ (show r)
compileSDef (id,e) =
  mapM_ (\(id,e) -> label id >> saving [_ra] (compileSExp e _v0) >> jr _ra) $
  (uncurry (:)) $
  flip evalState 0 $
  runWriterT $ 
  transfo e >>= \e'-> return $ (id,e')
                     
compileFile f =
  (map toLower) $
  (\(a,b) -> ".text:\n" ++ a ++ ".data:\n" ++ b) $
  flip evalState 0 $
  execWriterT $
  (compileSDef ("entrypoint",ELet undefined f (EIdent undefined "main"))) >> primitives

transfo (EOp l o) = return $ SEIdent $ "Core" ++ show o
transfo (EUnaryMinus _) = return $ SEIdent "CoreUnaryMinus"
transfo (EEList l) = return $ SEIdent "CoreEmptyList"
transfo (EConst l v) = return $ SEConst v
transfo (EIdent l i) = return $ SEIdent i
transfo (Return l) = return $ SEIdent "CoreReturn" 
transfo (ECase l) = return $ SEIdent "CoreCase"
transfo (EITE l x y z) =
  do x' <- transfo x
     y' <- transfo y
     z' <- transfo z
     return $ foldl SEApp (SEIdent "ITE") [x',y',z']
transfo (ELet l defs e) =
  do defs' <- mapM (onSndM transfo) defs
     e' <- transfo e
     let indexes = zipWith (\i (id,_) -> (i,id)) [1..] defs'
     let subst = subsituteIndexes ("",indexes) SELocal
     let defs'' = map (subst . snd) defs'
     return $ SELet defs'' (subst e')
       where onSndM f (x,y) =
               do y' <- f y
                  return (x,y')
transfo (EApp l x y) = do
  x' <- transfo x
  y' <- transfo y
  return $ SEApp x' y'
transfo (EAbstr l x e) =
  do e' <- transfo e
     let indexes = zip [0..] (S.toList ((freeIds  e') S.\\ (S.singleton x)))
     let e'' = subsituteIndexes (x,indexes) (const SEClotElem) e'
     i <- get
     modify (+1)
     let label = "fun" ++ (show i)
     tell [(label,e'')]
     return $ SEClos (map (SEIdent . snd) indexes) label

subsituteIndexes (x,l) c = 
  let f n s = if s == x
            then SEArg0
            else case find ((== s) . snd) l of
              Just (i,_) -> c n i
              Nothing -> SEIdent s
  in let recur n e = case e of
           (SEApp x y) -> SEApp (recur n x) (recur n y)
           (SEClos l id) -> SEClos (map (recur n) l) id
           (SEIdent id) -> f n id
           (SELet defs e) ->
             SELet (map (recur (n +1)) defs) (recur (n+1) e)
           x -> x
     in recur 0
 
     
caca e =
  evalState (runWriterT (transfo e)) 0

freeIds (SEApp x y) = S.union (freeIds x) (freeIds y)
freeIds (SEClos l _) = foldr S.union S.empty (map freeIds l) 
freeIds (SEIdent  x) = S.singleton x
freeIds (SELet defs e) =
  S.union
  (foldr S.union S.empty (map freeIds defs))
  (freeIds e)
freeIds _ = S.empty


{-
wrapper defs =
  head $
  map (\(text,dat) -> ".text:\n" ++ (concat $ intersperse "\n" text) ++ "\n.data:\n" ++ (concat $ intersperse "\n" dat)) $
  map (\(a,x) -> (a,(map toMIPS) *** (map toMIPS) $ flip evalState 0 $ execWriterT $  compile x _s7)) $
  uncurry (++) $ 
  flip evalState 0 $
  runWriterT $ 
  (mapM (\(id,e) -> transfo e >>= \e'-> return $ (id,e')) defs)
-}
