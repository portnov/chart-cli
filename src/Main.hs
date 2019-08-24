{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import System.Environment

import Types
import Chart
import Parser

main :: IO ()
main = do
  [path, outPath] <- getArgs
  let opts = ParseOptions True "\t"
  cht <- parseFile opts path
  print $ toPairs $ chtValues cht

  renderableToFile def outPath $ toRenderable $ makeChart Points cht
  return ()

