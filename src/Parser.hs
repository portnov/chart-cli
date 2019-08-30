{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad
import Data.Colour.Names
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text as A
import Data.Dates
import System.IO.Unsafe (unsafePerformIO)

import Types

now :: DateTime
now = unsafePerformIO getCurrentDateTime

parseText :: ParseOptions -> T.Text -> T.Text -> Either String ChartData
parseText opts title text = do
  let lines = filter (not . T.null) $
        case poLineEnding opts of
          LF -> T.lines text
          CR -> T.split (== '\r') text
          CRLF -> T.splitOn "\r\n" text
      splitLine line = filter (not . T.null) $ T.splitOn (poSeparator opts) line

      inputColsCount = length (splitLine firstLine)

      parseLine :: Int -> T.Text -> Either String [Value]
      parseLine lineNo line =
        let items = splitLine line
        in  if length items == inputColsCount
            then zipWithM (parseValue lineNo) [1..] items
            else Left $ "Line " ++ show lineNo ++ ": number of columns (" ++ show (length items) ++
                         ") is not equal to number of columns in the first line (" ++
                         show inputColsCount ++ ")."

      parseValue lineNo colNo s =
        case A.parseOnly (A.double <* A.endOfInput) s of
          Right value -> Right $ Number value
          Left numberErr ->
            case parseDateTime now (T.unpack s) of
              Right dt -> Right $ Date $ toLocalTime dt
              Left dateErr -> Left $ "Line " ++ show lineNo ++ ", column " ++ show colNo ++ ":\n" ++
                                "Can't parse `" ++ T.unpack s ++
                                "` as a number: " ++ numberErr ++
                                ";\nCan't parse it as a date/time value: " ++ show dateErr

      firstLine = head lines

      defHeader n = "Column " <> T.pack (show n)

      dataLines =
        if poHeader opts
          then tail lines
          else lines

  inputValues <- zipWithM parseLine [1..] dataLines

  let values = if poIndex opts
                  then zipWith (:) (map Index [1..]) inputValues
                  else inputValues
      
      colsCount =
        if poIndex opts
          then inputColsCount + 1
          else inputColsCount

      columnNames =
        if poHeader opts
          then if poIndex opts
                 then "Index" : splitLine firstLine
                 else splitLine firstLine
          else map defHeader [0 .. colsCount - 1]

      columns = map NumberColumn columnNames

  return $ ChartData {
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
  case parseText opts title text of
    Right chart -> return chart
    Left err -> fail err

