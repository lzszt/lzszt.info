module Main where

import System.Environment
import Web qualified

main :: IO ()
main = do
  [outputDir] <- getArgs
  Web.renderHtml outputDir Web.lzsztInfo
