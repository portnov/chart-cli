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
import CmdLine

main :: IO ()
main = do
  cmd <- parseCmdLine
  print cmd
  let path = cmdInput cmd
      outPath = cmdOutput cmd
  let opts = cmdParse cmd
  cht <- parseFile opts path
  
  let cht' =
        case cmdTitle cmd of
          Nothing -> cht
          Just title -> cht {chtTitle = T.pack title}

  renderableToFile def outPath $ toRenderable $ makeChart (cmdChart cmd) cht'
  return ()

