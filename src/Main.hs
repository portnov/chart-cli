{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import qualified Data.Text as T
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import System.Environment
import System.FilePath

import Types
import Chart
import Parser
import CmdLine

main :: IO ()
main = do
  cmd <- parseCmdLine
--   print cmd
  let path = cmdInput cmd
      outPath = cmdOutput cmd
  let opts = cmdParse cmd
  cht <- parseFile opts path
--   print $ toPairs $ chtValues cht
  
  let cht' =
        case cmdTitle cmd of
          Nothing -> cht
          Just title -> cht {chtTitle = T.pack title}

      chart = cht' {
                  chtForeground = cmdForeground cmd
                , chtBackground = cmdBackground cmd
                , chtLegend = cmdLegend cmd
              }

  let width = cmdWidth cmd
      height = cmdHeight cmd
      fileOpts = fo_size .~ (width, height)
                  $ fo_format .~ detectFormat outPath
                  $ def

  renderableToFile fileOpts outPath $ toRenderable $ makeChart (cmdChart cmd) chart
  return ()

