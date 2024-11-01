{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TQF.Control.Graph
    ( GraphResult
    , DependencyGraph
    , dependencyGraph
    , requestNode
    , readNode
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Data.Functor
import qualified Data.Map                     as Map

newtype GraphResult out = GraphResult
    { unGraphResult :: TMVar (Either SomeException out)
    }

data DependencyGraph inp out = DependencyGraph
    { graphNodes :: TVar (Map.Map inp (TMVar (Either SomeException out)))
    , runNode    :: DependencyGraph inp out -> inp -> IO out
    }

dependencyGraph :: Ord inp => (DependencyGraph inp out -> inp -> IO out) -> IO (DependencyGraph inp out)
dependencyGraph runNode = do
    graphNodes <- newTVarIO mempty
    return DependencyGraph{..}

requestNode :: Ord inp => DependencyGraph inp out -> inp -> IO (GraphResult out)
requestNode graph inp = do
    (var, alreadyRunning) <- atomically $ do
            mCurVal <- Map.lookup inp <$> readTVar (graphNodes graph)
            case mCurVal of
                Nothing -> do
                    tmVar <- newEmptyTMVar
                    modifyTVar (graphNodes graph) (Map.insert inp tmVar)
                    return (tmVar, False)
                (Just x) -> return (x, True)

    unless alreadyRunning $ void $ forkFinally (runNode graph graph inp) (atomically . putTMVar var)
    return $ GraphResult var

readNode :: GraphResult out -> IO out
readNode = fmap (either throw id) . atomically . readTMVar . unGraphResult
