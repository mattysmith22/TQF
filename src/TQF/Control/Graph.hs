{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TQF.Control.Graph
    ( GraphResult
    , DependencyGraph
    , GraphContext
    , dependencyGraph
    , requestNode
    , readNode
    , requestNodeCtx
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Data.List
import qualified Data.Map                     as Map
import           Data.Maybe

newtype GraphResult out = GraphResult
    { unGraphResult :: TMVar (Either SomeException out)
    }

data CycleDetected = CycleDetected
    deriving Show
instance Exception CycleDetected where

data GraphContext inp out = GraphContext
    { ctxCurNode :: inp
    , ctxGraph   :: DependencyGraph inp out
    }
data DependencyGraph inp out = DependencyGraph
    { graphNodes :: TVar (Map.Map inp (TMVar (Either SomeException out)))
    , runNode    :: GraphContext inp out -> inp -> IO out
    , nodeDeps   :: TVar (Map.Map inp [inp])
    }

dfsFrom :: Ord a => (a -> Bool) -> a -> Map.Map a [a] -> Bool
dfsFrom p n g = case Map.lookup n g of
    Nothing   -> False
    (Just xs) -> any (\x -> p x || dfsFrom p x g) xs

wouldCauseCycleIn :: Ord a => (a,a) -> Map.Map a [a] -> Bool
wouldCauseCycleIn (parent,child) g = dfsFrom (==parent) parent $ addEdge (parent,child) g

addEdge :: Ord a => (a,a) -> Map.Map a [a] -> Map.Map a [a]
addEdge (parent, child) = Map.insertWith union parent [child]

dependencyGraph :: Ord inp => (GraphContext inp out -> inp -> IO out) -> IO (DependencyGraph inp out)
dependencyGraph runNode = do
    graphNodes <- newTVarIO mempty
    nodeDeps <- newTVarIO mempty
    return DependencyGraph{..}

requestNode :: Ord inp => DependencyGraph inp out -> inp -> IO (GraphResult out)
requestNode = requestNode' Nothing

requestNodeCtx :: Ord inp => GraphContext inp out -> inp -> IO (GraphResult out)
requestNodeCtx GraphContext{..} = requestNode' (Just ctxCurNode) ctxGraph

requestNode' :: Ord inp => Maybe inp -> DependencyGraph inp out -> inp -> IO (GraphResult out)
requestNode' mCurNode graph inp = do
    res <- atomically $ do
        mCurVal <- Map.lookup inp <$> readTVar (graphNodes graph)
        case mCurVal of
            Nothing -> do
                tmVar <- newEmptyTMVar
                modifyTVar (graphNodes graph) (Map.insert inp tmVar)
                mapM_ (\curNode -> modifyTVar (nodeDeps graph) $ addEdge (curNode, inp)) mCurNode
                return $ Right (tmVar, False)
            (Just x) -> do
                causesCycle <- fromMaybe False <$> mapM (\curNode -> do
                    let edge = (curNode, inp)
                    stateTVar (nodeDeps graph) (\g -> if edge `wouldCauseCycleIn` g then (True, g) else (False, addEdge edge g))) mCurNode
                return $ if causesCycle then Left () else Right (x, True)
    case res of
        (Left _) -> throwIO CycleDetected
        (Right (var, alreadyRunning)) -> do
            unless alreadyRunning $ void $ forkFinally (runNode graph (GraphContext inp graph) inp) (atomically . putTMVar var)
            return $ GraphResult var


readNode :: GraphResult out -> IO out
readNode = atomically . readTMVar . unGraphResult >=> either throwIO return
