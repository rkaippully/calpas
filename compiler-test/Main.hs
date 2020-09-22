module Main where

import System.Directory (listDirectory)
import System.FilePath.Posix (isExtensionOf, replaceExtension, (</>))
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple (pShowNoColor)

import Calpas.Compiler.Parser


main :: IO ()
main = do
  srcFiles <- filter ("pas" `isExtensionOf`) <$> listDirectory testDataDir
  defaultMain $ goldenTests $ (testDataDir </>) <$> srcFiles

testDataDir :: FilePath
testDataDir = "compiler-test/data/golden"

goldenTests :: [FilePath] -> TestTree
goldenTests srcFiles = testGroup "Golden Tests" $ mkGoldenTest <$> srcFiles

mkGoldenTest :: FilePath -> TestTree
mkGoldenTest inFile = let outFile = replaceExtension inFile "output"
                          goldenFile = replaceExtension inFile "golden"
                 in goldenVsFile inFile goldenFile outFile (runTest inFile outFile)

runTest :: FilePath -> FilePath -> IO ()
runTest inFile outFile = parseFile inFile >>= writeFileLText outFile . either identity pShowNoColor
