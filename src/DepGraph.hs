module DepGraph where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad.State
import Control.Arrow
import Data.Maybe

type Label = String

type Graph = M.Map Label (S.Set Label)

emptyGraph :: M.Map k a
emptyGraph = M.empty

addEdge :: Graph -> Label ->  Label -> Graph
addEdge graph from to =
  let children = fromMaybe S.empty (M.lookup from graph) in
  let newChildren = S.insert to children in 
  M.insert to (fromMaybe S.empty (M.lookup to graph)) $ M.insert from newChildren graph
  
data Mark = NotVisited | Visited | InProgress deriving Eq


cycleDetectionFromWithState :: Graph -> Label -> State (M.Map Label Mark) Bool
cycleDetectionFromWithState graph l =
  let recur node =
        case node  of
          Visited -> return False
          InProgress -> return True
          NotVisited ->
            do
              let children = fromMaybe [] (S.toList <$> (M.lookup l graph)) 
              modify (M.insert l InProgress)
              resul <- foldl (liftM2 (||)) (return False) $ map (cycleDetectionFromWithState graph) children
              modify (M.insert l Visited)
              return resul
  in
   do marked <- get
      recur (fromMaybe NotVisited $ M.lookup l marked)
      
cycleDetection :: Graph -> Bool
cycleDetection graph =
  let cycles = map (cycleDetectionFromWithState graph) (M.keys graph) in
  evalState (foldl (liftM2 (||)) (return False) cycles) (M.empty)

topoSortWithState :: Graph -> Label -> State ((M.Map Label Mark),[Label]) () 
topoSortWithState graph l =
  do (marks,_) <- get
     if fromMaybe NotVisited (M.lookup l marks) == Visited
       then return ()
       else do let children = fromMaybe [] (S.toList <$> (M.lookup l graph))
               _ <- sequence $ map (topoSortWithState graph) children
               modify $ (M.insert l Visited) *** (l:)
               return ()

reverseGraph :: Graph -> Graph
reverseGraph graph =
  foldl (\g (x,y) -> addEdge g y x) (M.empty) $
  concat $
  M.elems $
  M.mapWithKey (\k s -> map ((,) k) (S.toList s)) graph 

roots :: Graph -> [Label]
roots graph =
  map fst $
  filter ((== S.empty) . snd) $
  M.assocs $ reverseGraph graph

topoSort :: Graph -> [Label]
topoSort graph =
  if (cycleDetection graph) then error "Circular Dependency, aborting."
  else
    reverse $ 
    snd $
    execState
    (sequence $
     map (topoSortWithState graph) $
     roots graph)
    (M.empty,[])

     
  
