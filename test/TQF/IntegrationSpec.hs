module TQF.IntegrationSpec
  ( spec
  ) where

import Test.Hspec
import System.Directory.Recursive
import System.FilePath
import Data.List (isSuffixOf, isInfixOf)
import qualified TQF.CodeGen                   as CodeGen
import qualified SQF.AST                       as SQF
import           TQF.CodeGen.Optimiser         as Optimiser
import TQF
import TQF.AST
import System.Directory.Extra (doesDirectoryExist, doesFileExist)
import Control.Exception
import Control.Monad
import Control.Exception.Extra (errorIO)

spec :: Spec
spec = parallel $ do
  files <- runIO (getDirFiltered (\x -> ((||) . (==".tqf") $ takeExtension x) <$> doesDirectoryExist x) ("test" </> "integration"))
  files' <- runIO $ filterM doesFileExist files
  describe "Integration tests" $
    mapM_ createTestCase files'

createTestCase :: FilePath -> Spec
createTestCase fp
  | "Error" `isSuffixOf` takeBaseName fp = it (testName fp) $ do
    expectedError <- readFile (fp -<.> "error")
    compilePath fp `shouldThrow` ((expectedError `isInfixOf`) . (displayException :: SomeException -> String))
  | otherwise = it (testName fp) $ do
    resolved <- compilePath fp
    let gen = SQF.prettyPrint $ optimiseCommandCall <$> CodeGen.codeGen resolved
    expected <- readFile (fp -<.> "sqf")
    gen `shouldBe` expected
  where
    testName :: FilePath -> String
    testName = snd . splitFileName . snd . splitFileName 

    compilePath :: FilePath -> IO (Module Resolved)
    compilePath = either errorIO return . typeCheckedModule <=< compileModule (("test"</>).("integration"</>).pathForModule) []