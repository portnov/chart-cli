{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Colour.Names
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Types

parseText :: ParseOptions -> T.Text -> T.Text -> ChartData
parseText opts title text =
  let lines = T.lines text
      splitLine line = T.splitOn (poSeparator opts) line
      -- TODO: error handling
      parseLine line = map (Number . read' . T.unpack) $ splitLine line

      read' s =
        case reads s of
          [(x, "")] -> x
          _ -> error $ "no parse: " ++ s

      firstLine = head lines

      defHeader n = "Column " <> T.pack (show n)

      dataLines =
        if poHeader opts
          then tail lines
          else lines

      inputValues = map parseLine dataLines

      values =
        if poIndex opts
          then zipWith (:) (map Index [1..]) inputValues
          else inputValues
      
      inputColsCount = length (splitLine firstLine)
      colsCount =
        if poIndex opts
          then inputColsCount + 1
          else inputColsCount

      columnNames =
        if poHeader opts
          then splitLine firstLine
          else map defHeader [0 .. colsCount - 1]

      columns = map NumberColumn columnNames

  in  ChartData {
          chtTitle = title
        , chtColumns = columns
        , chtBackground = white
        , chtForeground = black
        , chtLegend = True
        , chtValues = values
      }

parseFile :: ParseOptions -> Maybe FilePath -> IO ChartData
parseFile opts mbPath = do
  text <- case mbPath of
            Nothing -> TIO.getContents
            Just path -> TIO.readFile path
  let title = case mbPath of
                Nothing -> "stdin"
                Just path -> T.pack path
  return $ parseText opts title text

