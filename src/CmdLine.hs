
module CmdLine where

import qualified Data.Text as T
import Data.Char (toUpper)
import Options.Applicative
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import System.FilePath

import Types

pCmdLine :: Parser CmdLine
pCmdLine =
    CmdLine
      <$> strOption
            ( long "output"
              <> short 'o'
              <> metavar "OUTPUT.png"
              <> value "chart.png"
              <> help "write output to OUTPUT.png"
            )
      <*> pChart
      <*> pParseOpts
      <*> (optional $ strOption
            ( long "title"
              <> short 't'
              <> metavar "TITLE"
              <> help "set chart title to TITLE"
            )
          )
      <*> option auto
            ( long "width"
              <> short 'w'
              <> value 800
              <> metavar "WIDTH"
              <> showDefault
              <> help "specify chart width, in pixels"
            )
      <*> option auto
            ( long "height"
              <> short 'h'
              <> value 600
              <> metavar "HEIGHT"
              <> showDefault
              <> help "specify chart height, in pixels"
            )
      <*> strArgument (metavar "INPUT.txt")

pChart :: Parser ChartConfig
pChart =
    subparser (
           command "line" (info (pure Line) (progDesc "Make a line chart"))
        <> command "area" (info (pure Area) (progDesc "Make an area chart"))
        <> command "points" (info (pure Points) (progDesc "Make a points chart"))
        <> command "bar" (info pBar (progDesc "Make a bar chart"))
      )
  <|> pure Line

pBar :: Parser ChartConfig
pBar =
  Bar
    <$> (    flag' BarsStacked (long "stack")
         <|> flag' BarsClustered (long "cluster")
         <|> pure BarsStacked
        )

pParseOpts :: Parser ParseOptions
pParseOpts =
  ParseOptions
    <$> switch
          ( long "header"
            <> short '1'
            <> help "first line contains column headers"
          )
    <*> (T.pack <$> strOption
          ( long "delimiter"
            <> short 'd'
            <> metavar "CHAR"
            <> value "\t"
            <> help "specify fields delimiter ('\\t' by default)"
          )
        )

parseCmdLine :: IO CmdLine
parseCmdLine = execParser opts
  where
    opts = info (pCmdLine <**> helper)
             ( fullDesc
               <> progDesc "Make a chart"
               <> header "chart - plot charts from input data"
             )

detectFormat :: FilePath -> FileFormat
detectFormat path =
  case map toUpper (takeExtension path) of
    ".PNG" -> PNG
    ".SVG" -> SVG
    ".PS" -> PS
    ".PDF" -> PDF
    ext -> error $ "unsupported output file format: " ++ ext

