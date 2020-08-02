import System.Environment ( getArgs )

import Data.LLVM.Analysis.PointsTo.AllocatorProfile
import Data.LLVM.Analysis.PointsTo.Andersen
import Data.LLVM.Parse

main :: IO ()
main = do
  [ fname ] <- getArgs
  mm <- parseLLVMFile defaultParserOptions fname
  case mm of
    Left err -> putStrLn err
    Right m -> do
      let a = runPointsToAnalysis [standardCProfile] m
      -- viewPointsToGraph a
      savePointsToGraph a (fname ++ ".dot")
      return ()
