module Utils where

import Xenesis.Core.Analyzer

drawTree :: FilePath -> String -> String -> IO ()
drawTree sysFile mod sym = do
  sys <- loadSystem False sysFile 
  let t = useTree sys (name mod) (name sym)
  drawUseTree t

