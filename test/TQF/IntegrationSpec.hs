module TQF.IntegrationSpec
  ( spec
  ) where

import           Control.Exception
import           Control.Exception.Extra    (errorIO)
import           Control.Monad
import           Data.List                  (isInfixOf, isSuffixOf)
import qualified SQF.AST                    as SQF
import           System.Directory.Extra     (doesDirectoryExist, doesFileExist)
import           System.Directory.Recursive
import           System.FilePath
import           Test.Hspec
import           TQF
import           TQF.AST
import qualified TQF.CodeGen                as CodeGen
import           TQF.CodeGen.Optimiser      as Optimiser
import           TQF.Resolve

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
    let gen = SQF.prettyPrint $ fmap optimiseCommandCall $ CodeGen.toScript $ CodeGen.codeGen resolved
    expected <- readFile (fp -<.> "sqf")
    gen `shouldBe` expected
  where
    testName :: FilePath -> String
    testName = snd . splitFileName . snd . splitFileName

    compilePath :: FilePath -> IO (Module Resolved)
    compilePath = either errorIO return . typeCheckedModule <=< compileModule pathResolver (trivialResolver pathResolver) . pathToModule tqfDir

    tqfDir = "test"</>"integration"

    pathResolver = moduleToPath tqfDir
