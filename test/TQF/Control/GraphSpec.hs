module TQF.Control.GraphSpec
    ( spec
    ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import qualified Data.Array                  as Array
import           Data.Graph
import           Data.List
import           System.Timeout
import           Test.Hspec
import           TQF.Control.Graph

-- The TVar logs every time a node is added to, so we can use it to check whether sharing occurs
generateTestNode :: Graph -> IO (GraphContext Int (Tree Int) -> Int -> IO (Tree Int), TVar [Int])
generateTestNode g = do
    var <- newTVarIO []
    let f ctx inp = do
            let deps = g Array.! inp
            subResults <- mapM (requestNodeCtx ctx) deps >>= mapM readNode
            atomically $ modifyTVar var (inp:)
            return $ Node inp subResults
    return (f, var)

spec :: Spec
spec = do
    it "Should be able to compute a single node with no dependencies" $ do
        let g = Array.array (0,0) [(0,[])]
        (f, _) <- generateTestNode g
        dg <- dependencyGraph f
        res <- requestNode dg 0 >>= readNode
        res `shouldBe` Node 0 []
    it "Should be able to compute multiple nodes with no dependencies" $ do
        let g = Array.array (0,1) [(0,[]), (1,[])]
        (f, _) <- generateTestNode g
        dg <- dependencyGraph f
        ress <- mapM (requestNode dg) [0,1] >>= mapM readNode
        ress `shouldBe` [Node 0 [], Node 1 []]
    it "Should be able to compute the dependency of a node" $ do
        let g = Array.array (0,1) [(0,[1]), (1,[])]
        (f, _) <- generateTestNode g
        dg <- dependencyGraph f
        res <- requestNode dg 0 >>= readNode
        res `shouldBe` Node 0 [Node 1 []]
    it "Should be able to share dependencies" $ do
        let g = Array.array (0,2) [(0,[2]), (1,[2]), (2,[])]
        (f, tvar) <- generateTestNode g
        dg <- dependencyGraph f
        ress <- mapM (requestNode dg) [0,1] >>= mapM readNode
        actions <- sort <$> readTVarIO tvar
        ress `shouldBe` [Node 0 [Node 2 []], Node 1 [Node 2 []]]
        actions `shouldBe` [0,1,2] -- If sharing had failed there would be two 2s
    it "Should be able to detect cycles" $ do
        let g = Array.array (0,1) [(0,[1]), (1,[0])]
        (f, _) <- generateTestNode g
        dg <- dependencyGraph f
        timeout 1000000 (requestNode dg 0 >>= readNode) `shouldThrow` anyException
